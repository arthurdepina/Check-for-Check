import Data.Char (isDigit)
import Debug.Trace (trace)

-- Converte uma string em uma linha do tabuleiro
str_to_row :: String -> [String]
str_to_row [] = []
str_to_row (c:cs)
  | isDigit c = replicate (digit_to_int c) " " ++ str_to_row cs
  | otherwise = [c] : str_to_row cs
  where digit_to_int c = read [c] :: Int

-- Converte todas as strings de entrada em um tabuleiro de xadrez
str_to_matrix :: [String] -> [[String]]
str_to_matrix = map str_to_row

-- Imprime o tabuleiro de forma legível
print_matrix :: (Show a) => [[a]] -> IO ()
print_matrix matrix = mapM_ print matrix

-- Encontra a posição do rei branco
find_white_king :: [[String]] -> (Int, Int)
find_white_king board = find_king 0 board

-- Busca rei branco iterando pelas linhas do tabuleiro
find_king :: Int -> [[String]] -> (Int, Int)
find_king _ [] = error "Não encontrado"
find_king i (row:rows) =
  case find_in_row 0 i row of
    Just pos -> pos
    Nothing -> find_king (i+1) rows

-- Busca rei branco na linha atual
find_in_row :: Int -> Int -> [String] -> Maybe (Int, Int)
find_in_row _ _ [] = Nothing
find_in_row j i (c:cs)
  | c == "R" = Just (i, j)
  | otherwise = find_in_row (j+1) i cs


-- Verifica se o rei branco está sendo atacado por cima em sua coluna
king_up_column :: [[String]] -> (Int, Int) -> Bool
king_up_column board (row, col) = check_up row col
  where
    check_up (-1) _ = False  -- Chegou ao topo do tabuleiro sem encontrar xeques
    check_up r c
      | piece == "t" || piece == "d" = True  -- Encontrou uma torre ou dama preta
      | piece /= " " && piece /= "R" = False -- Encontrou um peça que protege o rei
      | otherwise = check_up (r-1) c         -- Continua a busca
      where
        piece = (board !! r) !! c


-- Verifica se o rei branco está sendo atacado por baixo em sua coluna
king_down_column :: [[String]] -> (Int, Int) -> Bool
king_down_column board (row, col) = check_down row col
  where
    check_down (8) _ = False  -- Chegou a base do tabuleiro sem encontrar xeques
    check_down r c
      | piece == "t" || piece == "d" = True  -- Encontrou uma torre ou dama preta
      | piece /= " " && piece /= "R" = False -- Encontrou um peça que protege o rei
      | otherwise = check_down (r+1) c       -- Continua a busca
      where
        piece = (board !! r) !! c


-- Verifica se o rei branco está sendo atacado na esquerda em sua linha
king_left_row :: [[String]] -> (Int, Int) -> Bool
king_left_row board (row, col) = check_left row col
  where
    check_left _ (-1) = False -- Saiu do tabuleiro pela esquerda
    check_left r c
      | piece == "t" || piece == "d" = True
      | piece /= " " && piece /= "R" = False
      | otherwise = check_left r (c-1)
      where
        piece = (board !! r) !! c


-- Verifica se o rei branco está sendo atacado na direita em sua linha
king_right_row :: [[String]] -> (Int, Int) -> Bool
king_right_row board (row, col) = check_right row col
  where
    check_right _ (8) = False
    check_right r c
      | piece == "t" || piece == "d" = True
      | piece /= " " && piece /= "R" = False
      | otherwise = check_right r (c+1)
      where
        piece = (board !! r) !! c


-- Verifica se o rei branco está sendo atacado na diagonal superior esquerda
king_upper_left_diag :: [[String]] -> (Int, Int) -> Bool
king_upper_left_diag board (row, col) = check_upper_left row col
  where
    check_upper_left _ (-1) = False
    check_upper_left (-1) _ = False
    check_upper_left r c
      | piece == "b" || piece == "d" = True
      | piece /= " " && piece /= "R" = False
      | otherwise = check_upper_left (r-1) (c-1)
      where
        piece = (board !! r) !! c


-- Verifica se o rei branco está sendo atacado na diagonal superior direita
king_upper_right_diag :: [[String]] -> (Int, Int) -> Bool
king_upper_right_diag board (row, col) = check_upper_right row col
  where
    check_upper_right _ (8)  = False
    check_upper_right (-1) _ = False
    check_upper_right r c
      | piece == "b" || piece == "d" = True
      | piece /= " " && piece /= "R" = False
      | otherwise = check_upper_right (r-1) (c+1)
      where
        piece = (board !! r) !! c


-- Verifica se o rei branco está sendo atacado na diagonal inferior esquerda
king_lower_left_diag :: [[String]] -> (Int, Int) -> Bool
king_lower_left_diag board (row, col) = check_lower_left row col
  where
    check_lower_left _ (-1) = False
    check_lower_left (8) _  = False
    check_lower_left r c
      | piece == "b" || piece == "d" = True
      | piece /= " " && piece /= "R" = False
      | otherwise = check_lower_left (r+1) (c-1)
      where
        piece = (board !! r) !! c


-- Verifica se o rei branco está sendo atacado na diagonal inferior direita
king_lower_right_diag :: [[String]] -> (Int, Int) -> Bool
king_lower_right_diag board (row, col) = check_lower_right row col
  where
    check_lower_right _ (8) = False
    check_lower_right (8) _ = False
    check_lower_right r c
      | piece == "b" || piece == "d" = True
      | piece /= " " && piece /= "R" = False
      | otherwise = check_lower_right (r+1) (c+1)
      where
        piece = (board !! r) !! c


-- Verifica se o rei branco está sendo atacado por algum cavalo preto
king_horse :: [[String]] -> (Int, Int) -> Bool 
king_horse board (row, col) = any is_black_horse positions
  where
    horse_moves = [(2,1), (2,-1), (-2,1), (-2,-1), (1,2), (1,-2), (-1,2), (-1,-2)]
    positions = map (\(dr, dc) -> (row + dr, col + dc)) horse_moves -- dr/dc = diferença em row/column
    is_black_horse (r,c)
      | r >= 0 && r < 8 && c >= 0 && c < 8 = (board !! r) !! c == "c"
      | otherwise = False


-- Verifica se o rei branco está sendo atacado por algum peão preto
king_pawns :: [[String]] -> (Int, Int) -> Bool
king_pawns board (row, col) = any is_black_pawn positions
  where
    pawn_moves = [(-1, -1), (-1, 1)]
    positions = map (\(dr, dc) -> (row + dr, col + dc)) pawn_moves
    is_black_pawn (r,c)
      | r >= 0 && r < 8 && c >= 0 && c < 8 = (board !! r) !! c == "p"
      | otherwise = False

-- Verifica se o rei branco está em xeque
check_for_check :: [[String]] -> (Int, Int) -> Bool
check_for_check board kingPos = 
    king_up_column board kingPos        ||
    king_down_column board kingPos      ||
    king_left_row board kingPos         ||
    king_right_row board kingPos        ||
    king_upper_left_diag board kingPos  ||
    king_upper_right_diag board kingPos ||
    king_lower_left_diag board kingPos  ||
    king_lower_right_diag board kingPos ||
    king_horse board kingPos            ||
    king_pawns board kingPos


main :: IO ()
main = do
  let board = ["tcbdrbct","pppppppp","8","8","8","8","PPPPPPPP","TCBDRBCT"]
  let matrix_board = str_to_matrix board
  print_matrix matrix_board
  let king_pos_b = find_white_king matrix_board
  print king_pos_b

  -- Verifica se o rei está em xeque
  let in_check = check_for_check matrix_board king_pos_b
  print in_check
  
