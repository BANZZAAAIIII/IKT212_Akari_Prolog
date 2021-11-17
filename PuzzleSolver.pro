outputFile('.\\solved\\puzzle___.txt').
inputFile('.\\unsolved\\puzzleSolved_02.txt').

/********************* solving the puzzle */
doSolve(P, TB):-
	transpose(P, TB),
	!,
	%checkLines(TB),
	checkNums(TB),
	!.

% TODO: Split lines into before and after a wall, recursion
%!	checkLines(puzzle(size(_,_), board(B), tBoard(TB))) is det
%	Checks if two lights are intersecting on its row or column
checkLines(puzzle(size(_,_), board(B), tBoard(TB))):-
	filterBoard(B, NB),
	checkIntersection(NB),
	filterBoard(TB, NTB),
	checkIntersection(NTB),
	!.

checkIntersectionPos(Row, Col) :-
	checkList(Row),
	checkList(Col).

checkIntersection(Board) :-
	maplist(checkList, Board).

checkList(B) :- checkList(B, _).
checkList([], 0).
checkList(['*'|T],C):-
	checkList(T, D),
	C is D+1,			% Increment counter
	!,		
	C < 2.				% Two lights are intersecting when this fails

checkList([_|T], C) :- 
	checkList(T, D),
	C is D-D. % Reset counter, silly way due to instantiation

%!  transpose(puzzle(size(X,Y), board(B)), puzzle(size(X,Y), board(B), tBoard(TB))).
%   Transpose a matrix board
transpose(puzzle(size(Row,Column), board(B)), puzzle(size(Row,Column), board(B), tBoard(TB))):-
	trans(B, TB).
trans([],[]).
trans([[]|_], []):-!.
trans([S|R], [L|L1]) :-
    trans(S, R, L, M),
    trans(M, L1).

trans([], _,[],[]).
trans([S1|S2], [], [S1|L1], [S2|M]):-
    trans([], [], L1, M).
trans([S1|S2], [R1|R2], [S1|L1], [S2|M]):-
    trans(R1, R2, L1, M).



% Checks that number constraint for all num tiles are correct 
checkNums(Board) :- checkNums(Board, 1, 1).
checkNums(puzzle(size(Row, Col), board(B), tBoard(_)), CurrentRow, CurrentCol) :-
    %write("Row: "), write(CurrentRow), write(", "), write("Col: "), write(CurrentCol), nl, 
    getValue(B, CurrentCol, CurrentRow, V),
	%write("V: "), write(V), nl,
	checkIfNum(B, V, CurrentCol, CurrentRow), 
	(CurrentRow >= Row -> 
    	NextRow is 1, NextCol is CurrentCol + 1 ; 
    	NextRow is CurrentRow + 1, NextCol = CurrentCol
    ),
	checkNums(puzzle(size(Row, Col), board(B), tBoard(_)), NextRow, NextCol).
checkNums(puzzle(size(_, Y), board(_), tBoard(_)), _, CurrentY):-
	not(CurrentY =< Y).



checkIfNum(Board, Num, Row, Col) :-
	% write("Row: "), write(Row), write(", "), write("Col: "), write(Col), nl, 
	(integer(Num) -> % We only want to check if the tile is a number
    	% nl, 
		checkCorrectNrOfLights(Board, Num, Row, Col) ; 
    	true
    ).

% Checks that nr of lights around a num tile is valid
checkCorrectNrOfLights(Board, Num, Row, Col) :-
	getAdjacentTiles(Board, Row, Col, Tiles),
    countLights(Tiles, N), !,
	% write("Tiles: "), write(Tiles), nl,
    % write("Nr of lights: "), write(Num), write(" of "), write(N), nl,
	N = Num.
checkCorrectNrOfLights(_, '*', _, _).
checkCorrectNrOfLights(_, '_', _, _).
checkCorrectNrOfLights(_, _, _, _).


getAdjacentTiles(Board, X, Y, R) :-
    getAdjacentPos(X, Y, PosList),
    % write("adjc pos: "), write(PosList), nl,
	getTiles(Board, PosList, R).

getTiles(_, [], []).
getTiles(Board, [Pos|PosList], Result) :-
	getTiles(Board, PosList, Result1),
	% write("Pos: "), write(Pos), write(", List: "), write(PosList), nl,
	checkPos(Board, Pos, R),
	append(Result1, [R], Result).
getTiles(_, _, _).

getAdjacentPos(X, Y, R) :-
	X1 is X - 1,
	X2 is X + 1,
	Y1 is Y - 1,
	Y2 is Y + 1,
	append([], [[X1, Y], [X2, Y], [X, Y1], [X, Y2]], R).

checkPos(Board, [Col|[Row|_]], R) :-
    % write("check pos: "), write(Col), write(" "), write(Row), nl,
	getValue(Board, Col, Row, R).
checkPos(_, _, []).


countLights([],0).
countLights(['*'|T],N) :- countLights(T, N1), N is N1 + 1.
countLights([X|T],N) :- X \= '*', countLights(T,N).


getValue(Board, RowNum, ColNum, Val) :- 
    nth1(RowNum, Board, Row), nth1(ColNum, Row, Val).


row(puzzle(size(_, _), board(B), tBoard(_)), N, Row) :-
    nth1(N, B, Row).
col(puzzle(size(_, _), board(_), tBoard(BT)), N, Col) :-
    row(puzzle(size(_, _), board(BT), tBoard(_)), N, Col).


% TODO: Check if this can be abstracted so we can swap out the filter
is_empty(Tile) :-  dif(Tile, '_').
filterBoard(Board, NewBoard) :-
	maplist(filterLine, Board, NewBoard).
filterLine(Line, NewLine) :-
	include(is_empty, Line, NewLine).

/********************* writing the result */
writeFullOutput(puzzle(size(Row,Col), board(Grid), tBoard(_))):- 
	write("size "), write(Row), write("x"), write(Col), nl,
	writeBoard(Grid).
writeFullOutput(P):- write('Cannot solve puzzle: '), write(P), nl.

writeBoard([]).
writeBoard([H|T]):-
	writeLine(H),
	writeBoard(T).

writeLine([]):- nl.
writeLine([Head|Tail]):- 
	write(Head),
	writeLine(Tail).


/********************** reading the input */
readProblem(puzzle(size(Row,Col), board(Grid))) :- 
	findKW(size), 
	readInt(Row), 
	readInt(Col), 
	length(Grid, Col),
	readGridLines(Row,Grid).

findKW(KW):- 
	string_codes(KW,[H|T]), peek_code(H), readKW([H|T]), !.
findKW(_):- 
	peek_code(-1), !, fail.
findKW(KW):- 
	get_code(_), findKW(KW).


readKW([]):- 
	get_code(_).
readKW([H|T]):- 
	get_code(H), readKW(T).


readGridLines(_,[]).
readGridLines(N,[H|T]):- 
	length(H,N), 
	readGridLine(H), 
	readGridLines(N,T).

readGridLine([]).
readGridLine([E|T]):- 
	get_code(M), 
	translate(M,E), 
	!, 
	readGridLine(T).

translate(-1,'ERROR: EOF').
translate(63,_).
translate(X,E):- whitespace(X), get_code(Y), translate(Y,E).
translate(X,E):- name(E,[X]).


whitespace(10). whitespace(12). whitespace(32).


readHintLine(0).
readHintLine(N):-
	N>0, 
	N1 is N-1, 
	get_code(_), 
	readHintLine(N1).

readLines(_,0).
readLines(Row,Col):- 
	Col>0, 
	Col1 is Col-1,
	readHintLine(Row), 
	readLines(Row,Col1).


readInt(N):- 
	get_code(M),
	handleCode(M,N).


handleCode(M,N):- 
	is_number_code(M,N1),
	!,
	continueInt(N1,N).
handleCode(-1,_):- 
	!, 
	fail. /* EOF */
handleCode(_,N):- 
	readInt(N).


continueInt(O,N):- 
	get_code(M),
	is_number_code(M,M1),
	!,
	H is 10*O+M1,
	continueInt(H,N).
continueInt(N,N).


is_number_code(N, N1):- 
	N>=48,
	N<58,
	N1 is N-48.
is_number_code(95,0).

/*********************** global control: starting the algorithm and the reading */
input_output(IF,OF):- 
	current_prolog_flag(argv, ["--io",IF,OF]), !.
input_output(IF,OF):- 
	inputFile(IF),
	outputFile(OF).

run :- 
	input_output(IF, OF),
	write("IO: "), nl,
	write("\tReading from: "), 
	write(IF), nl,
	write("\twriting to: "), 
	write(OF), nl, 
	see(IF), 
	tell(OF), 
	findKW(puzzles), 
	readInt(N),
	write('puzzles '),
	write(N),
	nl, 
	solvePuzzles(N), 
	told, 
	seen,
	!.
	
run :- 
	told,
	seen. /* close the files */

solvePuzzles(0).
solvePuzzles(N) :- 
	N>0,
	readProblem(P),
	doSolve(P, S),
	writeFullOutput(S),
	!,
	N1 is N-1,
	solvePuzzles(N1).

%:- run.
%:- halt.
