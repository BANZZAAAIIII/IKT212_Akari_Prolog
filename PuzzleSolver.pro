outputFile('.\\solved\\puzzle_00.txt').
% inputFile('.\\unsolved\\puzzle_00.txt').
% inputFile('.\\unsolved\\puzzle_01.txt').
inputFile('.\\unsolved\\puzzle_02.txt').
% inputFile('.\\unsolved\\puzzleSolved_02.txt').



/********************************************/
/********************* solving the puzzle **/
/******************************************/
doSolve(InitialBoard, Board):- % puzzle(size(Row,Col), board(B), tBoard(TB), lines(L), walls(W))
	setupBoard(InitialBoard, Board), % puzzle(size(Col,Row), board(B), tBoard(TB), lines(L), walls(Walls), tiles(S))
	!, 
	checkLines(Board), !,
	checkNums(Board),
	% tempPlaceLight(Board),
	% placeLight(pos(1,4), Board),
	!.

tempPlaceLight(puzzle(size(Col,Row), board(B), tBoard(TB), lines(L), walls(Walls), tiles(S))) :-
	getValue(S, 2,1, Val),
	write(Val).

% placeLight(tile(value(?Tile), lines(Lines), walls(Walls)))
placeLight(tile(value(Tile), lines(Lines), walls(Walls))) :-
	var(Tile),
	write("Lines: "), write(Lines), nl,
	write("Walls: "), write(Walls), nl,
	% checkLines(Lines),
	% var(Val),
	% Val = "*",
	!.


% Checks all the line groups and fails if one has more than 1 light
checkLines(puzzle(size(Col,Row), board(B), tBoard(TB), lines(L), walls(Walls), tiles(S))) :- 
	maplist(countLightsLine, L).
countLightsLine(Line) :-
	countLightsWalls(Line, N),
	N < 2. % Fails when counting more than two lights


% Checks that number constraint for all num tiles are correct
checkNums(puzzle(size(_,_), board(_), tBoard(_), lines(_), walls(Walls), tiles(_))):- 
	% write("All Walls: "), write(Walls), nl,
	checkNum(Walls).
checkNum([]).
checkNum([[Num|Walls]|Tail]):- 
	% write("num: "), write(Num), write(", Walls: "), write(Walls), nl,
	checkCorrectNrOfLights(Num, Walls),
	checkNum(Tail).

% Checks that nr of lights around a num tile is valid
checkCorrectNrOfLights(Num, A) :-
	flatten(A, Adjacent),
    countLightsWalls(Adjacent, NrOfLights), !,
	% write("Tiles: "), write(Adjacent), nl,
    % write("Nr of lights: "), write(NrOfLights), write(" of "), write(Num), nl,
	NrOfLights == Num.

% Counts number if * in a list while ignoring/skipping free vars
countLightsWalls([],0).
countLightsWalls([X|T],N) :- not(var(X)), X == '*', countLightsWalls(T, N1), N is N1 + 1.
countLightsWalls([_|T],N) :- countLightsWalls(T,N).


setValue(Board, Row, RowNum, ColNum, Val, NewBoard) :-
	replace_nth1(Row, ColNum, Val, NewRow),
	replace_nth1(Board, RowNum, NewRow, NewBoard).

replace_nth1(List, Index, NewElem, NewList) :-
	% predicate works forward: Index,List -> OldElem, Transfer
	nth1(Index,List,_,Transfer),
	% predicate works backwards: Index,NewElem,Transfer -> NewList
	nth1(Index,NewList,NewElem,Transfer).

getValue(Board, RowNum, ColNum, Val) :-
    nth1(RowNum, Board, Row), nth1(ColNum, Row, Val).

% Returns True if given list is empty
is_empty(List) :- not(member(_, List)).

row(puzzle(size(_,_), board(B), tBoard(_), lines(_), walls(_)), N, Row) :-
    nth1(N, B, Row).
col(puzzle(size(_, _), board(_), trans(BT)), N, Col) :-
    row(puzzle(size(_, _), board(BT), trans(_), lines(_), walls(_)), N, Col).

getRow(B, N, R) :-
	nth1(N, B, R).
getCol(B, N, R) :-
	trans(B, TB),
	getRow(TB, N, R).


/***********************************************/
/********************* Setting Up the puzzle **/
/*********************************************/
% Transforms board to include additional datastructures
% Board     -  puzzle(size(Row,Col), board(B)
% NewBoard  -  puzzle(size(Row,Col), board(B), tBoard(TB), lines(L), walls(W))
setupBoard(Board, NewBoard):-
	transpose(Board, TransBoard),
	setupLines(TransBoard, LinesBoard),
	setupNums(LinesBoard, WallsBoard),
	setupSolver(WallsBoard, NewBoard),
	!.


% Adds tiles(S) to the datastructure
% This contains a free tiles on the board with its corresponding line and walls
setupSolver(puzzle(size(Col,Row), board(B), tBoard(TB), lines(Lines), walls(Walls)), puzzle(size(Col,Row), board(B), tBoard(TB), lines(L), walls(Walls), tiles(S))) :-
	createSolverMatrix(Col, Row, S),
	createSolverTiles(B, Walls, Lines, S), % List of tile(value(_), lines([line groups]))
	% write(S), nl,
	!.

createSolverMatrix(Col, Row, Board) :-
	length(Board, Row),
	createColumn(Board, Col),
	% write("Solver Matrix: "), write(Board),nl,
	!.
createColumn([], Col).
createColumn([H|T], Col) :-
	length(H, Col),
	createColumn(T, Col).


createSolverTiles([], Walls, Lines, []).
createSolverTiles([Bh|Bt], Walls, Lines, [Sh|St]) :- 
	% write("BoardLine: "), write(Bh),nl,
	% write("SolverLine: "), write(SH),nl,
	createSolverLine(Bh, Walls, Lines, Sh),		% Loop a line
	createSolverTiles(Bt, Walls, Lines, St).	% Next Row

createSolverLine([], Walls, Lines, []).
createSolverLine([Lh|Lt], Walls, Lines, [Rh|Rt]) :- 
	% write("Tile: "), write(Lh), nl, 
	checkLines(Lh, Lines, LineResult), 	% Check tile
	checkWalls(Lh, Walls, WallResult), 
	Rh = tile(value(Lh), lines(LineResult), walls(WallResult)),
	% write("Result: "), write(Result), nl,
	createSolverLine(Lt, Walls, Lines, Rt). % Next tile

checkLines(Tile, [], Result):-
	Result = [].
checkLines(Tile, [H|T], Result) :- 
	checkLines(Tile, T, Result1),
	% write("Line group: "), write(H), nl,
	(freeMember(Tile, H) ->
		% write("Success"), nl, 
		append([H], Result1, Result);
		% write("Failure"),
		Result = Result1,
		true, !
	).

checkWalls(Tile, [], Result) :-
	Result = [].
checkWalls(Tile, [H|T], Result) :- 
	checkWalls(Tile, T, Result1),
	nth1(2, H, WallTiles),
	(freeMember(Tile, WallTiles) ->
		append([H], Result1, Result);
		Result = Result1,
		true
	).

% Checks if Elem is the given list, works with free variables, if not in list fail
freeMember(Elem,[]):- !, fail.
freeMember(Elem, [H|T]) :-
	(Elem == H ->
		true;
		!, freeMember(Elem, T)
	),
	!.
% Checks if Elem is the given list, works with free variables, if not in list fail
freeMember(Elem,[]):- !, fail.
freeMember(Elem, [H|T]) :-
	(Elem == H ->
		true;
		!, freeMember(Elem, T)
	),
	!.


% Adds lines(L) to the datastructure
% This countains a list of all lines that are continuous
setupLines(puzzle(size(Col,Row), board(B), tBoard(TB)), puzzle(size(Col,Row), board(B), tBoard(TB), lines(L))) :- 
	findLines(B, Lines),
	findLines(TB, TLines),
	append(Lines, TLines, L),
	!.

findLines([], Result).
findLines([H|T], Result) :- 
	findLines(T, Result1),
	splitLine(H, Line, Result2), 
	(is_empty(Line) ->
		Result3 = Result2;
		append([Line], Result2, Result3)
	),
	(var(Result1) ->
		Result = Result3;
		append(Result3, Result1, Result)
	).
splitLine([], Line, Result) :-
	Line = [],
	Result = [].
splitLine([H|T],  Line, Result) :- 
	splitLine(T, Line1, Result1),
	% write(", Val: "), write(H), write(", Line: "), write(Line1), write(", Lines: "), write(Result1), nl,
	(var(H) ->
		append([H], Line1, Line),
		Result = Result1;
		(is_empty(Line1) ->
			Result = Result1;
			append([Line1], Result1, Result)
		),
		Line = []
	).

% Adds walls(W) to the datastructure
% This countains a list of all number walls with its adjacent tiles
setupNums(puzzle(size(Row,Col), board(B), tBoard(TB), lines(L)), NewBoard):-
	findNums(B, Row, Col, Row, Col, R),
	% write("Walls: "), write(R), nl,
	addWallsToStruct(puzzle(size(Row,Col), board(B), tBoard(TB), lines(L)), R, NewBoard).

addWallsToStruct(puzzle(size(Row,Col), board(B), tBoard(TB), lines(L)), Walls, puzzle(size(Row,Col), board(B), tBoard(TB), lines(L), walls(Walls))).

findNums(_, _, _, 1, 1, Result):-
	getValue(Board, 1, 1, Val),
	getAdjacentIfNum(Board, 1, 1, Val, AdjacentList), 
	%write("Col: "), write(1), write(", Row: "), write(1), write(", Val: "), write(Val), nl,
	append(R, AdjacentList, Result).
findNums(Board, Col, Row, 1, CurrentRow, Result):- 
	R1 is CurrentRow - 1,
	findNums(Board, Col, Row, Col, R1, R),
	getValue(Board, CurrentRow, 1, Val),
	getAdjacentIfNum(Board, CurrentRow, 1, Val, AdjacentList),
	%write("Col: "), write(1), write(", Row: "), write(CurrentRow), write(", Val: "), write(Val), nl,
	append(R, AdjacentList, Result).
findNums(Board, Col, Row, CurrentCol, CurrentRow, Result):-
	C1 is CurrentCol - 1,
	findNums(Board, Col, Row, C1, CurrentRow, R),
	getValue(Board, CurrentRow, CurrentCol, Val),
	getAdjacentIfNum(Board, CurrentRow, CurrentCol, Val, AdjacentList),
	%write("Col: "), write(CurrentCol), write(", Row: "), write(CurrentRow), write(", Val: "), write(Val), nl,
	append(R, AdjacentList, Result).


getAdjacentIfNum(Board, Col, Row, Num, List) :-
	%write("Row: "), write(Row), write(", "), write("Col: "), write(Col), nl,
	(integer(Num) -> % We only want to check if the tile is a number
		getAdjacentTiles(Board, Col, Row, R),
		append([], [[Num, R]], List)
		;
		List = [],
		true
    ).

getAdjacentTiles(Board, Col, Row, R) :-
    getAdjacentPositions(Col, Row, PosList),
    % write("adjc pos: "), write(PosList), nl,
	getTiles(Board, PosList, R).

getTiles(_, [], []).
getTiles(Board, [Pos|PosList], Result) :-
	getTiles(Board, PosList, Result1),
	%write("Pos: "), write(Pos), write(", List: "), write(PosList), nl,
	checkPos(Board, Pos, R),
	!,
	append(Result1, [R], Result).

checkPos(Board, [Col|[Row|_]], R) :-
    % write("check pos: "), write(Col), write(" "), write(Row), nl,
	getValue(Board, Col, Row, R).
checkPos(_, _, []).

getAdjacentPositions(X, Y, R) :-
	X1 is X - 1, X2 is X + 1,
	Y1 is Y - 1, Y2 is Y + 1,
	append([], [[X1, Y], [X2, Y], [X, Y1], [X, Y2]], R).

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



/********************************************/
/********************* writing the result **/
/******************************************/
writeFullOutput(puzzle(size(Row,Col), board(B), tBoard(TB), lines(L), walls(Walls), tiles(S))):-
	write("size "), write(Row), write("x"), write(Col), nl,
	writeBoard(B), nl,
	write("size "), write(Col), write("x"), write(Row), write(", Transpose"), nl,
	writeBoard(TB).
writeFullOutput(P):- write('Cannot solve puzzle: '), write(P), nl.

writeBoard([]).
writeBoard([H|T]):-
	writeLine(H),
	writeBoard(T).

writeLine([]):- nl.
writeLine([Head|Tail]):-
	(var(Head) ->
		write('_');
		write(Head)
	),
	writeLine(Tail).



/********************************************/
/********************** reading the input **/
/******************************************/
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
translate(95, A).
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



/**********************************************************************************/
/*********************** global control: starting the algorithm and the reading **/
/********************************************************************************/
input_output(IF,OF):-
	current_prolog_flag(argv, ["--io",IF,OF]), !.
input_output(IF,OF):-
	inputFile(IF),
	outputFile(OF).

run :-
	input_output(IF, OF),
	write("IO: "), nl,
	write("\tReading from: "), write(IF), nl,
	write("\twriting to: "), write(OF), nl,
	see(IF),
	tell(OF),
	findKW(puzzles),
	readInt(N),
	write('puzzles '), write(N), nl,
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

% :- run.
%:- halt.
