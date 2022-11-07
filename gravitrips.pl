%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Project: 	Final Project Mamam17
% Course:	20596 - Prolog & Artificial Intelligence
% Student:	Anton Dehtiar
% File Name: gravitrips.pl
% Game: Four in a Row
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Game board description
% ==========================================================================================
% The game board is represented as a list:
% createBoard(Board).
% Board representation:	
% 			| .. |  - empty spot
%			  r - red human's spot	
%			  y - yellow computer's spot (max Player)  
% Star: y starts the game
% Each list item represents 1 cell.
% The board size if represented by row(6)/col(7) as requiered for this game by default,
% It is possible to add an option to require the user to choose NxM ,
% P.S: I will add it if I have time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

row(6).
col(7).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% play().
% ==========================================================================================
% Start the game
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play():- 
	write('Enter Game Difficulty MaxDepth:'),nl ,
    read_int(MaxDepth),
	create_board(Board), 
    game(Board, MaxDepth). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% game(+Board, +MaxDepth)
% ==========================================================================================
% Yellow player (AI) start the game.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

game(Board, MaxDepth):-
	alphabeta(y, Board, -3000, 3000, MaxDepth, 0, NextMove, _), 
    write('Player: AI - yellow'),nl ,
	display_board(NextMove), 
	(((check_victory(NextMove, y),!) ; (draw(NextMove),!));
	update_bord_by_r(NextMove, NextMove1), % Player rotation (to red - human)
	write('Player: Human - red'),nl ,
    display_board(NextMove1),
    (check_victory(NextMove1, r),!;
	(draw(NextMove1),!; 
    game(NextMove1, MaxDepth)))). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  create_board(-Board)
% ==========================================================================================
%  list of Row-Col-free
%  
%  board_size(N,M).
%  board_size(row,col).
%  
%  Generated Board Example:
%  [7-6-free, 6-6-free, 5-6-free, 4-6-free, 3-6-free, 2-6-free, 1-6-free,
%   7-5-free, 6-5-free, 5-5-free, 4-5-free, 3-5-free, 2-5-free, 1-5-free, 
%   7-4-free, 6-4-free, 5-4-free, 4-4-free, 3-4-free, 2-4-free, 1-4-free, 
%   7-3-free, 6-3-free, 5-3-free, 4-3-free, 3-3-free, 2-3-free, 1-3-free, 
%   7-2-free, 6-2-free, 5-2-free, 4-2-free, 3-2-free, 2-2-free, 1-2-free, 
%   7-1-free, 6-1-free, 5-1-free, 4-1-free, 3-1-free, 2-1-free, 1-1-free]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_board(Board):-
    row(N),
    col(M),
	create_board(N, M, Board). % N-rows M-columns

create_board( 0, _, []):-!.

% Create board row by row
create_board(Row, Col, Rows):-
	create_row(Row, Col, Row_i),
	Row1 is Row-1,
	create_board(Row1, Col, Rows1),
    append(Row_i, Rows1, Rows).

create_row(_, 0, []):-!.

% Create board Cell by cell
create_row(Row, Col, [Cell|Cells]):-
	%Column-Row-State
	Cell = Col-Row-free,
	Col1 is Col-1,
	create_row(Row, Col1, Cells).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% update_bord_by_r(+Board, -NewBoard)
% ==========================================================================================
% Requiered for rotate player in Game predicat
% Insert red ( Human ) player row in the cell ant return updated board ( Next Move )
% When user choose row coins descends to the lowest possible cell in this row
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_bord_by_r(Board, NewBoard):-
	col(Col),
	repeat,
    (
    	format("Please select the required column between 1-~d:~n", Col),  
		read(User_Input), 
    	((User_Input==quit,!, fail) ; 
    		(% move return false if user input not 1-7 column
            move(Board, r, NewBoard, User_Input),!;
    		write("Incorrect Choose, try again"), nl, fail
          )
        )
    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% display_board(+Board)
% ==========================================================================================
% Show board in terminal -> row by row -> cell by cell
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_board(Board):-
    col(M), row(N),
    tab(6),
	display_column_index(M), nl,
	display_board(Board, N, M). % display rows

display_board(_, 0, _):-!.

% Display row by row 
% N-rows M-columns
display_board(Board, N, M):-
    tab(6), write( '----------------------------' ), nl ,
    display_row_index(N),
    tab(3),
    write( '|' ),
	display_row(N, M, Board),
    nl,
	N1 is N-1,
	display_board(Board, N1, M),
	nl.

display_row(_, 0, _):-!.

% Display row by Cell after Cell
display_row(N, M, Board):-
	member(M-N-State, Board),		% N-rows M-columns
    (
    	(State==free, write(" .. "),!); % free s is a double dots
		write(" "),write(State)),write(" "), write("|"), 
		M1 is M-1,
		display_row(N, M1, Board
     ).

display_column_index(0):-!.

display_column_index(Col):-
    write(Col), 
    tab(3),
	Col1 is Col-1,
	display_column_index(Col1).

display_row_index(0):-!.

display_row_index(N):-
    write(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% draw(+Board)
% ==========================================================================================
% There are no free cells left then Draw
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

draw(Board):-
	not(member(_-_-free, Board)),
    write("Draw!"),nl.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% Alphabeta algorithm implementation %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% alphabeta(+Player, +Board, +Alpha, +Beta, +MaxDepth, Depth, -NewBoard, -Val)
% ==========================================================================================
% Based on figure 24.5 implementation alphabeta algorithm (p.585)
% The way of detection min/max r/y player.
%
% y - yellow AI
% r - red Human
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

opponent(y, r).
opponent(r, y).

alphabeta(Player, Board, Alpha, Beta, MaxDepth, Depth, GoodMove, Val) :-
    %  victory true then no requiered more iteractions
    victory(Board, Player), !, 		
	((Player==y, Val=3000) ; 		% Return Score
	(Player==r, Val= -3000))		% Return Score
    ;
    % Main alfa-beta like in the book
	Depth < MaxDepth,
	NewDepth is Depth + 1,
	moves(Board, Player, Moves),	% collect possible legal move to moves
    opponent(Player, NextPlayer),	% Rotate player
	boundedbest(NextPlayer, Moves, Alpha, Beta, MaxDepth, NewDepth, GoodMove, Val)
    ;   
    % When we reached maximum depth
	% evaluation function evaluate(+Board, -Eval).
    Depth = MaxDepth,
	!, evaluate(Board, Val).


boundedbest(Player, [Move|Moves], Alpha, Beta, MaxDepth, Depth, GoodMove, GoodVal) :- 
	alphabeta(Player, Move, Alpha, Beta, MaxDepth, Depth, _, Val),
	goodenough(Player, Moves, Alpha, Beta, Move, Val, MaxDepth, Depth, GoodMove, GoodVal).

goodenough(_, [], _, _, Move, Val, _, _, Move, Val) :- !.  % No other candidate

% divided into two parts  
% in original goodenough min_to_move(+Pos) p.585 in Book
goodenough(Player, _, Alpha, Beta, Move, Val, _, _, Move, Val) :-
	min_to_move(Player),Val > Beta,! % Maximizer attainded upper bound
    ;   
	max_to_move(Player), Val < Alpha,!.% Minimizer attained lower bound


goodenough(Player, Moves, Alpha, Beta, Move, Val, MaxDepth, Depth, GoodMove, GoodVal) :-
	newbounds(Player, Alpha, Beta, Move, Val, NewAlpha, NewBeta),	% Refine bounds
	boundedbest(Player, Moves, NewAlpha, NewBeta, MaxDepth, Depth, GoodMove1, GoodVal1),
	betterof(Player, Move, Val, GoodMove1, GoodVal1, GoodMove, GoodVal).

newbounds(Player, Alpha, Beta, _, Val, Val, Beta) :-
	min_to_move(Player),	
    Val > Alpha,!. 		% Maximizer increased lower bound

newbounds(Player, Alpha, Beta, _, Val, Alpha, Val) :-
	max_to_move(Player),	
    Val < Beta, !.		 	% Minimizer decreased upper bound

newbounds(_, Alpha, Beta, _, _, Alpha, Beta). % otherwise bounds unchanged

% betterof(Player, Move, Val, GoodMove1, GoodVal1, GoodMove, GoodVal).
betterof(Player, Pos, Val, _, Val1, Pos, Val):- % Pos better than Pos1
	min_to_move(Player),	% MIN to move in Pos0
    Val > Val1,!			% MAX prefers the greater value
	;
	max_to_move(Player),	% MAX to move in Pos0
    Val < Val1,!.			% MIN prefers the lesser value

betterof(_, _, _, Pos1, Val1, Pos1, Val1). % Otherwise Pos1 better

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% min_to_move(player)
% ==========================================================================================
% True if the next player to play is the "red" player.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

min_to_move(r).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% max_to_move(Pos)
% ==========================================================================================
% True if the next player to play is the "yellow" player.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

max_to_move(y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% moves(+Board, +Player, -Moves)
% ==========================================================================================
% Return all moves from current state
% 
% setof(+Template, +Goal, -Set) Equivalent to bagof/3, 
% but sorts the result using sort/2 to get a sorted list of alternatives
% without duplicates.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

moves(Board, Player, Moves):-
    opponent(Player,_),
    % just collect all possible legal moves
	setof(M, Col^move(Board, Player, M, Col), Moves).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% move(+Board, +Player, -NewBoard, +Col)
% ==========================================================================================
% Get one possible move if Col is var, and checks if it's 
% Return single correct move ( NewBoard )
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

move(Board, Player, NewBoard, Col):- 
    row(N),
	member(Col-Row-free, Board), Row=<N, 
	(Row==1 ;
	(Row1 is Row-1, member(Col-Row1-Taken, Board), Taken\=free)),% Row befor
    delete(Board, Col-Row-free, NewBoard1),
    append(NewBoard1, [Col-Row-Player], NewBoard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Evaluation function
% ==========================================================================================
% Comb_Score is calculated by Score_y - Score_y.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

evaluate(Board, Eval):-
	calculate(Board, y, Score_y), 
	calculate(Board, r, Score_r),
	Eval is Score_y - Score_r.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% calculate(+Board, +Player, -Score)
% aggregate_all(sum(Score1),+choose_four(+Board,+four_pos(Position),+Player,+Score1), -Res)
% ==========================================================================================
% Score of a player is the sum:
% Scores of: horizontal_four +vertical_four + diagonal_four ==> DiagR+DiagL+Hor+Ver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

calculate(Board, Player, Score):-
	aggregate_all(sum(Score1), choose_four(Board, four_pos(diagonal_r), Player, Score1), DiagR),
	aggregate_all(sum(Score2), choose_four(Board, four_pos(diagonal_l), Player, Score2), DiagL),
	aggregate_all(sum(Score3), choose_four(Board, four_pos(horizontal), Player, Score3), Hor),
	aggregate_all(sum(Score4), choose_four(Board, four_pos(vertical), Player, Score4), Ver),
	Score is DiagR+DiagL+Hor+Ver.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% choose_four(+Board, +Rule, +Player, -Score)
% ==========================================================================================
% Choose four cells with same color
% Return the Score
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

choose_four(Board, Rule, Player, Score):-
	call(Rule, Board, Q),
    free_filled_cells(Q, Player, Free, Filled),
	mapScore(Free, Filled, Score).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% victory(+Board, +Player)
% ==========================================================================================
% Checks all of the Four options if they providing Victory
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

victory(Board, Player):-
	four_pos(diagonal_r, Board, Four),
	four_of_the_same_color(Four, Player),!.
victory(Board, Player):-
	four_pos(diagonal_l, Board, Four),
	four_of_the_same_color(Four, Player),!.
victory(Board, Player):-
	four_pos(horizontal, Board, Four),
	four_of_the_same_color(Four, Player),!.
victory(Board, Player):-
	four_pos(vertical, Board, Four),
	four_of_the_same_color(Four, Player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% four_of_the_same_color(+[Color, Color, Color, Color], -Color)
% ==========================================================================================
% Color equal red player/ yellow player
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

four_of_the_same_color([Color, Color, Color, Color], Color):- % Color equal red player/ yellow player
	Color \= free.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mapScore(+Free, +Filled, -Score)
% ==========================================================================================
% Map records according to the number of free and filled cells
% One Free with three filled equal 2
% Two Free with Two filled equal 1
% Returen Score
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mapScore(Free, Filled, Score):-
    Free = 1,
    Filled = 3,
    Score is 2,!
    ;
    Free = 2,
    Filled = 2,
    Score is 1,!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% free_filled_cells(+[free|Rest], +Player, -Free, -Filled)
% ==========================================================================================
% Sumurize how many of free cells and full sells together in the curent four.
% If the opposite player's node is encountered, it fails.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

free_filled_cells([], _, 0, 0):-!.

free_filled_cells([free|Rest], Player, Free, Filled):-
	free_filled_cells(Rest, Player, Free1, Filled),
	Free is Free1+1,!. 

free_filled_cells([Player|Rest], Player, Free, Filled):-
	free_filled_cells(Rest, Player, Free, Filled1),
    Filled is Filled1+1,!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% four_pos(+Position, +Board, -[Cell1, Cell2, Cell3, Cell4])
% ==========================================================================================
% Return all correct four coins position combinations 
% Row-Col-free
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

four_pos(Position, Board, [Cell1, Cell2, Cell3, Cell4]):-
    Position = diagonal_r,
	member(Row1-Col1-Cell1, Board), Col2 is Col1+1, Row2 is Row1+1,
	member(Row2-Col2-Cell2, Board), Col3 is Col2+1, Row3 is Row2+1,
	member(Row3-Col3-Cell3, Board), Col4 is Col3+1, Row4 is Row3+1,
	member(Row4-Col4-Cell4, Board);
    
    Position = diagonal_l,
    member(Row1-Col1-Cell1, Board), Col2 is Col1+1, Row2 is Row1-1,
	member(Row2-Col2-Cell2, Board), Col3 is Col2+1, Row3 is Row2-1,
	member(Row3-Col3-Cell3, Board), Col4 is Col3+1, Row4 is Row3-1,
	member(Row4-Col4-Cell4, Board);
    
    Position = horizontal,
	member(Row1-Col-Cell1, Board), Row2 is Row1+1,
	member(Row2-Col-Cell2, Board), Row3 is Row2+1, 
	member(Row3-Col-Cell3, Board), Row4 is Row3+1, 
	member(Row4-Col-Cell4, Board);

    Position = vertical,
	member(Row-Col1-Cell1, Board), Col2 is Col1+1,
	member(Row-Col2-Cell2, Board), Col3 is Col2+1, 
	member(Row-Col3-Cell3, Board), Col4 is Col3+1, 
	member(Row-Col4-Cell4, Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check_victory(+Board, +Player)
% ==========================================================================================
% Check if this board will cause y (AI) to win or loose
% Write the result as output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_victory(Board, Player):-
    Player = y ,
	victory(Board, y),	% Check if this board will cause y (AI) to win
    write("AI Win The Game!"),nl,!;
    Player = r ,
    victory(Board, r),	% check that a move by Player wont cause AI to lose
    write("AI Loose The Game!"),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% read_int(+X)
% ==========================================================================================
% read int
% read again if input is incorrect
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Reads an integer input
read_int(X):-read(X),integer(X),!. % Cut used to stop when an integer is read
read_int(X):-writeln('Error'),read_int(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  End  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
