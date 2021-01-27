//Novice is a port of the Butter chess engine to C.
//Various adjustments/functions from the Video Instruction Chess Engine.
//It also has various enhancements to alphaBeta from CeeChess.

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include <windows.h>
#include <intrin.h>
#include "math.h"

typedef unsigned long long U64;
typedef char string[200];

#define NAME "Novice 1.0"

#define NUM_SQUARES 64
#define MAX_GAME_MOVES 2048
#define MAX_DEPTH 64
#define MAX_POSITION_MOVES 256

#define STARTING_FEN "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

#define INFINITE 30000
#define ISMATE (INFINITE - MAX_DEPTH)

const U64 FILE_A_MASK = 0x0101010101010101;
const U64 NOT_FILE_A_MASK = 0xFEFEFEFEFEFEFEFE;
const U64 NOT_FILE_H_MASK = 0x7F7F7F7F7F7F7F7F;
const U64 RANK_1_MASK = 0x00000000000000FF;
const U64 RANK_3_MASK = 0x0000000000FF0000;
const U64 RANK_6_MASK = 0x0000FF0000000000;
const U64 RANK_8_MASK = 0xFF00000000000000;
const U64 NOT_RANK_8_MASK = 0x00FFFFFFFFFFFFFF;

enum {
    WHITE, BLACK,
    WHITE_PAWN, WHITE_KNIGHT, WHITE_BISHOP, WHITE_ROOK, WHITE_QUEEN, WHITE_KING,
    BLACK_PAWN, BLACK_KNIGHT, BLACK_BISHOP, BLACK_ROOK, BLACK_QUEEN, BLACK_KING,
};

enum { FILE_A, FILE_B, FILE_C, FILE_D, FILE_E, FILE_F, FILE_G, FILE_H };
enum { RANK_1, RANK_2, RANK_3, RANK_4, RANK_5, RANK_6, RANK_7, RANK_8 };
enum { WHITE_KING_CASTLE = 1, WHITE_QUEEN_CASTLE = 2, BLACK_KING_CASTLE = 4, BLACK_QUEEN_CASTLE = 8 };

enum {
    A1, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8,
};

typedef struct {
    int move;
    int score;
} Move;

typedef struct {
    Move moves[MAX_POSITION_MOVES];
    int count;
}MoveList;

typedef struct {
    U64 posKey;
    int move;
    int score;
    int depth;
    int flags;
} HashEntry;

typedef struct {
    HashEntry* pTable;
    int numEntries;

    int newWrite;
    int overWrite;
    int hit;
    int cut;
} HashTable;

typedef struct {
    int move;

    int castlePermissions;
    int fiftyMoveCounter;
    int enPassantSquare;
    U64 posKey;
} Undo;

typedef struct {
    U64 pieceBB[14];
    U64 emptyBB, occupiedBB;

    int side;
    int ply, histPly;

    int enPassantSquare;
    int castlePermissions;
    int fiftyMoveCounter;
    U64 posKey;
    Undo history[MAX_GAME_MOVES];

    HashTable HashTable[1];
    int pvArray[MAX_DEPTH];

    int searchHistory[14][64];
    int searchKillers[2][MAX_DEPTH];
} Board;

typedef struct {
    int startTime;
    int endTime;
    int depth;

    int depthSet;
    int timeSet;
    int movesToGo;

    long nodes;

    int infinite;
    int quit;
    int stopped;

    float failHigh;
    float failHighFirst;
    int nullCut;
} SearchInfo;

enum { NORTH, NORTH_EAST, EAST, SOUTH_EAST, SOUTH, SOUTH_WEST, WEST, NORTH_WEST };

enum {
    FLAG_NONE, FLAG_ALPHA, FLAG_BETA, FLAG_EXACT
};

const int pawnTableEarly[64] = {
   0,   0,   0,   0,   0,   0,   0,   0,
  10,  10,   0, -10, -10,   0,  10,  10,
   5,   0,   0,   5,   5,   0,   0,   5,
   0,   0,  10,  20,  20,  10,   0,   0,
   5,   5,   5,  10,  10,   5,   5,   5,
  10,  10,  10,  20,  20,  10,  10,  10,
  20,  20,  20,  30,  30,  20,  20,  20,
   0,   0,   0,   0,   0,   0,   0,   0
};
const int pawnTableLate[64] = {
   0,   0,   0,   0,   0,   0,   0,   0,
  -5,  -5,  -5,  -5,  -5,  -5,  -5,  -5,
   0,   0,   0,   0,   0,   0,   0,   0,
   5,   5,   5,   5,   5,   5,   5,   5,
  10,  10,  10,  10,  10,  10,  10,  10,
  15,  15,  15,  15,  15,  15,  15,  15,
  20,  20,  20,  20,  20,  20,  20,  20,
   0,   0,   0,   0,   0,   0,   0,   0,
};

const int knightTableEarly[64] = {
   0, -10,   0,   0,   0,   0, -10,   0,
   0,   0,   0,   5,   5,   0,   0,   0,
   0,   0,  10,  10,  10,  10,   0,   0,
   0,   5,  10,  20,  20,  10,   5,   0,
   5,  10,  15,  20,  20,  15,  10,   5,
   5,  10,  10,  20,  20,  10,  10,   5,
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0
};
const int knightTableLate[64] = {
 -10,  -5,  -5,  -5,  -5,  -5,  -5, -10,
 -10,  -5,   0,   0,   0,   0,  -5, -10,
 -10,  -5,   0,   5,   5,   0,  -5, -10,
  -5,   0,   5,  10,  10,   5,   0,  -5,
   0,   5,  10,  15,  15,  10,   5,   0,
   0,   5,  10,  15,  15,  10,   5,   0,
   0,   0,   5,  10,  10,   5,   0,   0,
  -5,   0,   0,   0,   0,   0,   0,  -5
};

const int bishopTableEarly[64] = {
   0,   0, -10,   0,   0, -10,   0,   0,
   0,  10,   0,   5,   5,   0,  10,   0,
   0,   0,  10,  10,  10,  10,   0,   0,
   0,   5,  10,  20,  20,  10,   5,   0,
   5,  10,  15,  20,  20,  15,  10,   5,
   5,  10,  10,  20,  20,  10,  10,   5,
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0
};
const int bishopTableLate[64] = {
   0,   0,  -5, -10, -10,  -5,   0,   0,
   0,   5,   0,   0,   0,   0,   5,   0,
   0,   0,  10,  10,  10,  10,   0,   0,
   0,   5,  15,  15,  15,  15,   5,   0,
   5,  10,  15,  20,  20,  15,  10,   5,
   5,  10,  10,  20,  20,  10,  10,   5,
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0
};

const int rookTableEarly[64] = {
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   5,  10,  10,   5,   0,   0,
  25,  25,  25,  25,  25,  25,  25,  25,
   0,   0,   5,  10,  10,   5,   0,   0
};
const int rookTableLate[64] = {
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   5,  10,  10,   5,   0,   0,
  25,  25,  25,  25,  25,  25,  25,  25,
   0,   0,   5,  10,  10,   5,   0,   0
};

const int queenTableEarly[64] = {
  -5,  -5,  -5,  -5,  -5,  -5,  -5,  -5,
   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   5,   5,   5,   5,   0,   0,
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   5,  10,  10,   5,   0,   0,
   0,   0,   5,   5,   5,   5,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0,
   0,   0,   0,   0,   0,   0,   0,   0
};
const int queenTableLate[64] = {
  -5,  -5,  -5,  -5,  -5,  -5,  -5,  -5,
  -5,   0,   0,   0,   0,   0,   0,  -5,
  -5,   0,  10,  10,  10,  10,   0,  -5,
  -5,   0,  10,  20,  20,  10,   0,  -5,
  -5,   0,  10,  20,  20,  10,   0,  -5,
   0,   0,  10,  15,  15,  10,   0,   0,
  20,  20,  20,  20,  20,  20,  20,  20,
   0,   0,   5,  10,  10,   5,   0,   0
};

const int kingTableEarly[64] = {
   5,   5,  25,   0,   0,   0,  25,   5,
  -5,  -5,  -5,  -5,  -5,  -5,  -5,  -5,
 -10, -10, -10, -10, -10, -10, -10, -10,
 -10, -10, -10, -10, -10, -10, -10, -10,
 -10, -10, -10, -10, -10, -10, -10, -10,
 -10, -10, -10, -10, -10, -10, -10, -10,
 -10, -10, -10, -10, -10, -10, -10, -10,
 -10, -10, -10, -10, -10, -10, -10, -10
};
const int kingTableLate[64] = {
 -10,  -5,  -5,  -5,  -5,  -5,  -5, -10,
  -5,  -5,  -5,  -5,  -5,  -5,  -5,  -5,
   5,   5,  10,  10,  10,  10,   5,   5,
   5,   5,  10,  10,  10,  10,   5,   5,
  10,  10,  15,  20,  20,  15,  10,  10,
   5,   5,  10,  10,  10,  10,   5,   5,
   5,   5,  10,  10,  10,  10,   5,   5,
   0,   0,   0,   0,   0,   0,   0,   0
};

const int mirror[64] = {
  56,  57,  58,  59,  60,  61,  62,  63,
  48,  49,  50,  51,  52,  53,  54,  55,
  40,  41,  42,  43,  44,  45,  46,  47,
  32,  33,  34,  35,  36,  37,  38,  39,
  24,  25,  26,  27,  28,  29,  30,  31,
  16,  17,  18,  19,  20,  21,  22,  23,
   8,   9,  10,  11,  12,  13,  14,  15,
   0,   1,   2,   3,   4,   5,   6,   7
};

const int pieceValue[14] = { 0, 0, 100, 325, 325, 550, 1000, 50000, 100, 325, 325, 550, 1000, 50000 };

const U64 FLAG_CA = 0x1000000;
const U64 FLAG_EP = 0x2000000;
const U64 FLAG_PS = 0x4000000;

#define generateMove(f, t, ca, mov, pro, flags) ((f) | ((t) << 6) | ((ca) << 12) | ((mov) << 16) | ((pro) << 20) | flags)

#define from(m) ((m) & 0x3F)
#define to(m) (((m) >> 6) & 0x3F)
#define captured(m) (((m) >> 12) & 0xF)
#define moving(m) (((m) >> 16) & 0xF)
#define promoted(m) (((m) >> 20) & 0xF)

#define isCastle(m) ((m) & 0x1000000)
#define isEnPassant(m) ((m) & 0x2000000)
#define isPawnStart(m) ((m) & 0x4000000)
#define isCapture(m) ((m) & 0xF000)
#define isPromote(m) ((m) & 0xF00000)

#define NOMOVE 0

/* MACROS */

#define getSquare(file, rank) (((rank) * 8) + (file))
#define onBoard(file, rank) (file >= FILE_A && file <= FILE_H && rank >= RANK_1 && rank <= RANK_8)

#define popBit(bb) ((bb) &= ((bb) - 1))
#define setBit(bb,sq) ((bb) |= setMask[(sq)])
#define clearBit(bb,sq) ((bb) &= clearMask[(sq)])
#define bitCount(bb) ((int) (__popcnt64(bb)))

/* GLOBALS */

U64 setMask[65];
U64 clearMask[65];

int fileArray[64];
int rankArray[64];

U64 knightAttacks[64];
U64 kingAttacks[64];
U64 rookAttacksEmpty[64];
U64 bishopAttacksEmpty[64];

U64 bitRays[8][64];

U64 pieceKeys[14][64];
U64 sideKey;
U64 castleKeys[16];
U64 enPassantKeys[64];

//initialize

#define RAND_64 	((U64)rand() | \
					(U64)rand() << 15 | \
					(U64)rand() << 30 | \
					(U64)rand() << 45 | \
					((U64)rand() & 0xf) << 60 )

void initBitMasks() {
    for (int index = 0; index < 64; index++) {
        setMask[index] = 1ULL << index;
        clearMask[index] = ~setMask[index];
    }
    setMask[64] = 0ULL;
    clearMask[64] = ~setMask[64];
}

void initFileRankArrays() {
    for (int square = 0; square < 64; square++) {
        fileArray[square] = square % 8;
        rankArray[square] = square / 8;
    }
}

void initKnightAttacks() {
    for (int square = 0; square < 64; square++) {
        int originalFile = fileArray[square];
        int originalRank = rankArray[square];

        int file, rank;
        knightAttacks[square] = 0ULL;

        file = originalFile + 2; rank = originalRank + 1;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
        rank = originalRank - 1;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
        file = originalFile + 1; rank = originalRank + 2;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
        rank = originalRank - 2;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
        file = originalFile - 1; rank = originalRank + 2;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
        rank = originalRank - 2;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
        file = originalFile - 2; rank = originalRank + 1;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
        rank = originalRank - 1;
        if (onBoard(file, rank)) setBit(knightAttacks[square], getSquare(file, rank));
    }
}

void initKingAttacks() {
    for (int square = 0; square < 64; square++) {
        int file = fileArray[square];
        int rank = rankArray[square];

        kingAttacks[square] = 0ULL;

        if (onBoard(file + 1, rank + 1)) setBit(kingAttacks[square], getSquare(file + 1, rank + 1));
        if (onBoard(file + 1, rank)) setBit(kingAttacks[square], getSquare(file + 1, rank));
        if (onBoard(file + 1, rank - 1)) setBit(kingAttacks[square], getSquare(file + 1, rank - 1));
        if (onBoard(file, rank + 1)) setBit(kingAttacks[square], getSquare(file, rank + 1));
        if (onBoard(file, rank - 1)) setBit(kingAttacks[square], getSquare(file, rank - 1));
        if (onBoard(file - 1, rank + 1)) setBit(kingAttacks[square], getSquare(file - 1, rank + 1));
        if (onBoard(file - 1, rank)) setBit(kingAttacks[square], getSquare(file - 1, rank));
        if (onBoard(file - 1, rank - 1)) setBit(kingAttacks[square], getSquare(file - 1, rank - 1));
    }
}

void initRookAttacks() {
    for (int square = 0; square < 64; square++) {
        rookAttacksEmpty[square] = 0ULL;

        int file = fileArray[square];
        int rank = rankArray[square];

        for (int f = file + 1; f <= FILE_H; f++) {
            setBit(rookAttacksEmpty[square], getSquare(f, rank));
        }
        for (int f = file - 1; f >= FILE_A; f--) {
            setBit(rookAttacksEmpty[square], getSquare(f, rank));
        }
        for (int r = rank + 1; r <= RANK_8; r++) {
            setBit(rookAttacksEmpty[square], getSquare(file, r));
        }
        for (int r = rank - 1; r >= RANK_1; r--) {
            setBit(rookAttacksEmpty[square], getSquare(file, r));
        }
    }
}

void initBishopAttacks() {
    for (int square = 0; square < 64; square++) {
        bishopAttacksEmpty[square] = 0ULL;

        for (int tr = square + 9; (tr % 8 > 0) && (tr < 64); tr += 9) {
            setBit(bishopAttacksEmpty[square], tr);
        }
        for (int tl = square + 7; (tl % 8 < 7) && (tl < 64); tl += 7) {
            setBit(bishopAttacksEmpty[square], tl);
        }
        for (int br = square - 7; (br % 8 > 0) && (br >= 0); br -= 7) {
            setBit(bishopAttacksEmpty[square], br);
        }
        for (int bl = square - 9; (bl % 8 < 7) && (bl >= 0); bl -= 9) {
            setBit(bishopAttacksEmpty[square], bl);
        }
    }
}

void initBitRays() {
    for (int i = 0; i < 64; i++) {
        bitRays[NORTH][i] = 0ULL;
        bitRays[NORTH_EAST][i] = 0ULL;
        bitRays[EAST][i] = 0ULL;
        bitRays[SOUTH_EAST][i] = 0ULL;
        bitRays[SOUTH][i] = 0ULL;
        bitRays[SOUTH_WEST][i] = 0ULL;
        bitRays[WEST][i] = 0ULL;
        bitRays[NORTH_WEST][i] = 0ULL;
        for (int sq = i + 8; sq < 64; sq += 8) {
            setBit(bitRays[NORTH][i], sq);
        }
        for (int sq = i + 9; sq < 64; sq += 9) {
            if (fileArray[sq] == FILE_A) break;
            setBit(bitRays[NORTH_EAST][i], sq);
        }
        for (int sq = i + 1; sq < 64; sq += 1) {
            if (fileArray[sq] == FILE_A) break;
            setBit(bitRays[EAST][i], sq);
        }
        for (int sq = i - 7; sq >= 0; sq -= 7) {
            if (fileArray[sq] == FILE_A) break;
            setBit(bitRays[SOUTH_EAST][i], sq);
        }
        for (int sq = i - 8; sq >= 0; sq -= 8) {
            setBit(bitRays[SOUTH][i], sq);
        }
        for (int sq = i - 9; sq >= 0; sq -= 9) {
            if (fileArray[sq] == FILE_H) break;
            setBit(bitRays[SOUTH_WEST][i], sq);
        }
        for (int sq = i - 1; sq >= 0; sq -= 1) {
            if (fileArray[sq] == FILE_H) break;
            setBit(bitRays[WEST][i], sq);
        }
        for (int sq = i + 7; sq < 64; sq += 7) {
            if (fileArray[sq] == FILE_H) break;
            setBit(bitRays[NORTH_WEST][i], sq);
        }
    }
}

void initHashKeys() {

    for (int i = 0; i < 14; i++) {
        for (int j = 0; j < 64; j++) {
            pieceKeys[i][j] = RAND_64;
        }
    }
    sideKey = RAND_64;
    for (int i = 0; i < 16; i++) {
        castleKeys[i] = RAND_64;
    }
    for (int i = 0; i < 64; i++) {
        enPassantKeys[i] = RAND_64;
    }
}

int LMRTable[64][64];

void InitSearch() {
    // creating the LMR table entries (idea from Ethereal)
    for (int moveDepth = 1; moveDepth < 64; moveDepth++)
        for (int played = 1; played < 64; played++)
            LMRTable[moveDepth][played] = 1 + (log(moveDepth) * log(played) / 1.75);
}

void initAll() {
    initBitMasks();
    initFileRankArrays();

    initKnightAttacks();
    initKingAttacks();
    initRookAttacks();
    initBishopAttacks();

    initBitRays();

    initHashKeys();
    InitSearch();
}

//bitboard tools

void printBitBoard(U64 bb) {
    for (int square = 56; square >= 0; square++) {
        if (setMask[square] & bb) printf(" X ");
        else printf(" - ");
        if ((square + 1) % 8 == 0) {
            square -= 16;
            printf("\n");
        }
    }
}

int peekBit(U64 bb) {
    unsigned long index = -1;
    _BitScanForward64(&index, bb);
    return index;
}

int peekBitReverse(U64 bb) {
    unsigned long index = -1;
    _BitScanReverse64(&index, bb);
    return index;
}

//attacks

U64 rookAttacks(const Board* position, int sq) {
    U64 occ = position->occupiedBB | 0x8000000000000001;
    int n = peekBit(occ & (bitRays[NORTH][sq] | setMask[63]));
    int e = peekBit(occ & (bitRays[EAST][sq] | setMask[63]));
    int s = peekBitReverse(occ & (bitRays[SOUTH][sq] | setMask[0]));
    int w = peekBitReverse(occ & (bitRays[WEST][sq] | setMask[0]));
    return rookAttacksEmpty[sq] ^ bitRays[NORTH][n] ^ bitRays[EAST][e] ^ bitRays[SOUTH][s] ^ bitRays[WEST][w];
}

U64 bishopAttacks(const Board* position, int sq) {
    U64 occ = position->occupiedBB | 0x8000000000000001;
    int nw = peekBit(occ & (bitRays[NORTH_WEST][sq] | setMask[63]));
    int ne = peekBit(occ & (bitRays[NORTH_EAST][sq] | setMask[63]));
    int sw = peekBitReverse(occ & (bitRays[SOUTH_WEST][sq] | setMask[0]));
    int se = peekBitReverse(occ & (bitRays[SOUTH_EAST][sq] | setMask[0]));
    return bishopAttacksEmpty[sq] ^ bitRays[NORTH_WEST][nw] ^ bitRays[NORTH_EAST][ne] ^ bitRays[SOUTH_WEST][sw] ^ bitRays[SOUTH_EAST][se];
}

int attackedByWhite(const Board* position, int sq) {
    if (knightAttacks[sq] & position->pieceBB[WHITE_KNIGHT]) return 1;
    if (bishopAttacks(position, sq) & (position->pieceBB[WHITE_BISHOP] | position->pieceBB[WHITE_QUEEN])) return 1;
    if (rookAttacks(position, sq) & (position->pieceBB[WHITE_ROOK] | position->pieceBB[WHITE_QUEEN])) return 1;
    if ((((position->pieceBB[WHITE_PAWN] << 7) & NOT_FILE_H_MASK) | ((position->pieceBB[WHITE_PAWN] << 9) & NOT_FILE_A_MASK)) & setMask[sq]) return 1;
    if (kingAttacks[sq] & position->pieceBB[WHITE_KING]) return 1;
    return 0;
}

int attackedByBlack(const Board* position, int sq) {
    if (knightAttacks[sq] & position->pieceBB[BLACK_KNIGHT]) return 1;
    if (bishopAttacks(position, sq) & (position->pieceBB[BLACK_BISHOP] | position->pieceBB[BLACK_QUEEN])) return 1;
    if (rookAttacks(position, sq) & (position->pieceBB[BLACK_ROOK] | position->pieceBB[BLACK_QUEEN])) return 1;
    if ((((position->pieceBB[BLACK_PAWN] >> 9)& NOT_FILE_H_MASK) | ((position->pieceBB[BLACK_PAWN] >> 7)& NOT_FILE_A_MASK))& setMask[sq]) return 1;
    if (kingAttacks[sq] & position->pieceBB[BLACK_KING]) return 1;
    return 0;
}

int underCheck(const Board* position, int side) {
    if (side == WHITE) return attackedByBlack(position, peekBit(position->pieceBB[WHITE_KING]));
    else return attackedByWhite(position, peekBit(position->pieceBB[BLACK_KING]));
}

//move generator

void unmakeMove(Board* position);

void addQuietMove(const Board* position, MoveList* list, const int fromSquare, const int toSquare, const int movingPiece, const int promotingPiece, const int flags) {
    int move = generateMove(fromSquare, toSquare, 0, movingPiece, promotingPiece, flags);
    int score; int j;
    if (position->searchKillers[0][position->ply] == move) {
        score = 8000000;
    }
    else if (position->searchKillers[1][position->ply] == move) {
        score = 7000000;
    }
    else {
        score = position->searchHistory[movingPiece][toSquare];
    }
    //list->moves[list->count++] = { move, score };
    j = list->count++;
    list->moves[j].move = move;
    list->moves[j].score = score;
}

void addCaptureMove(const Board* position, MoveList* list, const int fromSquare, const int toSquare, const int capturedPiece, const int movingPiece, const int promotingPiece, const int flags) {
    int move = generateMove(fromSquare, toSquare, capturedPiece, movingPiece, promotingPiece, flags);
    int score = pieceValue[capturedPiece] - pieceValue[movingPiece] + 10000000;
    int j;
    //list->moves[list->count++] = { move, score};
    j = list->count++;
    list->moves[j].move = move;
    list->moves[j].score = score;
}

int getWhitePieceAt(const Board* position, U64 targetSquare) {
    for (int piece = WHITE_PAWN; piece <= WHITE_KING; piece++) {
        if ((position->pieceBB[piece] & targetSquare) != 0ULL) return piece;
    }
    return -1;
}

int getBlackPieceAt(const Board* position, U64 targetSquare) {
    for (int piece = BLACK_PAWN; piece <= BLACK_KING; piece++) {
        if ((position->pieceBB[piece] & targetSquare) != 0ULL) return piece;
    }
    return -1;
}

static void generatorRoutineWhite(const Board* position, int piece, MoveList* list) {
    U64 pieceBoard = position->pieceBB[piece];
    
    int fromIndex; int i = 0;
    while (pieceBoard) {
        fromIndex = peekBit(pieceBoard); popBit(pieceBoard);

        U64 attackBoard;
        switch (piece) {
        case WHITE_KNIGHT: attackBoard = knightAttacks[fromIndex]; break;
        case WHITE_BISHOP: attackBoard = bishopAttacks(position, fromIndex); break;
        case WHITE_ROOK: attackBoard = rookAttacks(position, fromIndex); break;
        case WHITE_QUEEN: attackBoard = bishopAttacks(position, fromIndex) | rookAttacks(position, fromIndex); break;
        }

        int toIndex;
        U64 moves = attackBoard & position->emptyBB;
        while (moves) {
            toIndex = peekBit(moves); popBit(moves);
            addQuietMove(position, list, fromIndex, toIndex, piece, 0, 0);
        }

        U64 attacks = attackBoard & position->pieceBB[BLACK];
        while (attacks) {
            toIndex = peekBit(attacks); popBit(attacks);
            int capturedPiece = getBlackPieceAt(position, setMask[toIndex]);
            addCaptureMove(position, list, fromIndex, toIndex, capturedPiece, piece, 0, 0);
        }
    }
}
static void generatorRoutineWhiteCaptures(const Board* position, int piece, MoveList* list) {
    U64 pieceBoard = position->pieceBB[piece];
    while (pieceBoard) {
        int fromIndex = peekBit(pieceBoard); popBit(pieceBoard);
        U64 attackBoard;
        switch (piece) {
        case WHITE_KNIGHT: attackBoard = knightAttacks[fromIndex]; break;
        case WHITE_BISHOP: attackBoard = bishopAttacks(position, fromIndex); break;
        case WHITE_ROOK: attackBoard = rookAttacks(position, fromIndex); break;
        case WHITE_QUEEN: attackBoard = bishopAttacks(position, fromIndex) | rookAttacks(position, fromIndex); break;
        }

        U64 attacks = attackBoard & position->pieceBB[BLACK];
        while (attacks) {
            int toIndex = peekBit(attacks); popBit(attacks);
            int capturedPiece = getBlackPieceAt(position, setMask[toIndex]);
            addCaptureMove(position, list, fromIndex, toIndex, capturedPiece, piece, 0, 0);
        }
    }
}
static void generatorRoutineBlack(const Board* position, int piece, MoveList* list) {
    U64 pieceBoard = position->pieceBB[piece];
    int fromIndex;
    while (pieceBoard) {
        fromIndex = peekBit(pieceBoard); popBit(pieceBoard);

        U64 attackBoard;
        switch (piece) {
        case BLACK_KNIGHT: attackBoard = knightAttacks[fromIndex]; break;
        case BLACK_BISHOP: attackBoard = bishopAttacks(position, fromIndex); break;
        case BLACK_ROOK: attackBoard = rookAttacks(position, fromIndex); break;
        case BLACK_QUEEN: attackBoard = bishopAttacks(position, fromIndex) | rookAttacks(position, fromIndex); break;
        }

        int toIndex;
        U64 moves = attackBoard & position->emptyBB;
        while (moves) {
            toIndex = peekBit(moves); popBit(moves);
            addQuietMove(position, list, fromIndex, toIndex, piece, 0, 0);
        }

        U64 attacks = attackBoard & position->pieceBB[WHITE];
        while (attacks) {
            toIndex = peekBit(attacks); popBit(attacks);
            int capturedPiece = getWhitePieceAt(position, setMask[toIndex]);
            addCaptureMove(position, list, fromIndex, toIndex, capturedPiece, piece, 0, 0);
        }
    }
}
static void generatorRoutineBlackCaptures(const Board* position, int piece, MoveList* list) {
    U64 pieceBoard = position->pieceBB[piece];
    while (pieceBoard) {
        int fromIndex = peekBit(pieceBoard); popBit(pieceBoard);
        U64 attackBoard;
        switch (piece) {
        case BLACK_KNIGHT: attackBoard = knightAttacks[fromIndex]; break;
        case BLACK_BISHOP: attackBoard = bishopAttacks(position, fromIndex); break;
        case BLACK_ROOK: attackBoard = rookAttacks(position, fromIndex); break;
        case BLACK_QUEEN: attackBoard = bishopAttacks(position, fromIndex) | rookAttacks(position, fromIndex); break;
        }

        U64 attacks = attackBoard & position->pieceBB[WHITE];
        while (attacks) {
            int toIndex = peekBit(attacks); popBit(attacks);
            int capturedPiece = getWhitePieceAt(position, setMask[toIndex]);
            addCaptureMove(position, list, fromIndex, toIndex, capturedPiece, piece, 0, 0);
        }
    }
}

static void generateWhitePawnMoves(const Board* position, MoveList* list) {
    U64 advance1 = (position->pieceBB[WHITE_PAWN] << 8) & position->emptyBB;
    U64 advance2 = ((advance1 & RANK_3_MASK) << 8) & position->emptyBB;
    int to;
    while (advance1 & NOT_RANK_8_MASK) {
        to = peekBit(advance1 & NOT_RANK_8_MASK); popBit(advance1);
        addQuietMove(position, list, to - 8, to, WHITE_PAWN, 0, 0);
    }
    while (advance1) {
        to = peekBit(advance1); popBit(advance1);
        int from = to - 8;
        addQuietMove(position, list, from, to, WHITE_PAWN, WHITE_QUEEN, 0);
        addQuietMove(position, list, from, to, WHITE_PAWN, WHITE_KNIGHT, 0);
        addQuietMove(position, list, from, to, WHITE_PAWN, WHITE_BISHOP, 0);
        addQuietMove(position, list, from, to, WHITE_PAWN, WHITE_ROOK, 0);
    }
    while (advance2) {
        to = peekBit(advance2); popBit(advance2);
        addQuietMove(position, list, to - 16, to, WHITE_PAWN, 0, FLAG_PS);
    }

    U64 leftAttacks = (position->pieceBB[WHITE_PAWN] << 7) & NOT_FILE_H_MASK & (position->pieceBB[BLACK] | setMask[position->enPassantSquare]);
    while (leftAttacks & NOT_RANK_8_MASK) {
        to = peekBit(leftAttacks & NOT_RANK_8_MASK); popBit(leftAttacks);
        int from = to - 7;
        if (to == position->enPassantSquare) {
            addCaptureMove(position, list, from, to, BLACK_PAWN, WHITE_PAWN, 0, FLAG_EP);
        }
        else {
            int captured = getBlackPieceAt(position, 1ULL << to);
            addCaptureMove(position, list, from, to, captured, WHITE_PAWN, 0, 0);
        }
    }
    while (leftAttacks) {
        to = peekBit(leftAttacks); popBit(leftAttacks);
        int from = to - 7;
        int captured = getBlackPieceAt(position, 1ULL << to);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_QUEEN, 0);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_KNIGHT, 0);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_BISHOP, 0);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_ROOK, 0);
    }

    U64 rightAttacks = (position->pieceBB[WHITE_PAWN] << 9) & NOT_FILE_A_MASK & (position->pieceBB[BLACK] | setMask[position->enPassantSquare]);
    while (rightAttacks & NOT_RANK_8_MASK) {
        to = peekBit(rightAttacks & NOT_RANK_8_MASK); popBit(rightAttacks);
        int from = to - 9;
        if (to == position->enPassantSquare) {
            addCaptureMove(position, list, from, to, BLACK_PAWN, WHITE_PAWN, 0, FLAG_EP);
        }
        else {
            int captured = getBlackPieceAt(position, 1ULL << to);
            addCaptureMove(position, list, from, to, captured, WHITE_PAWN, 0, 0);
        }
    }
    while (rightAttacks) {
        to = peekBit(rightAttacks); popBit(rightAttacks);
        int from = to - 9;
        int captured = getBlackPieceAt(position, 1ULL << to);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_QUEEN, 0);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_KNIGHT, 0);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_BISHOP, 0);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_ROOK, 0);
    }
}
static void generateWhitePawnCaptures(const Board* position, MoveList* list) {
    int to;
    U64 leftAttacks = (position->pieceBB[WHITE_PAWN] << 7) & NOT_FILE_H_MASK & (position->pieceBB[BLACK] | setMask[position->enPassantSquare]);
    while (leftAttacks & NOT_RANK_8_MASK) {
        to = peekBit(leftAttacks & NOT_RANK_8_MASK); popBit(leftAttacks);
        int from = to - 7;
        if (to == position->enPassantSquare) {
            addCaptureMove(position, list, from, to, BLACK_PAWN, WHITE_PAWN, 0, FLAG_EP);
        }
        else {
            int captured = getBlackPieceAt(position, 1ULL << to);
            addCaptureMove(position, list, from, to, captured, WHITE_PAWN, 0, 0);
        }
    }
    while (leftAttacks) {
        to = peekBit(leftAttacks); popBit(leftAttacks);
        int from = to - 7;
        int captured = getBlackPieceAt(position, 1ULL << to);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_QUEEN, 0);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_KNIGHT, 0);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_BISHOP, 0);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_ROOK, 0);
    }

    U64 rightAttacks = (position->pieceBB[WHITE_PAWN] << 9) & NOT_FILE_A_MASK & (position->pieceBB[BLACK] | setMask[position->enPassantSquare]);
    while (rightAttacks & NOT_RANK_8_MASK) {
        to = peekBit(rightAttacks & NOT_RANK_8_MASK); popBit(rightAttacks);
        int from = to - 9;
        if (to == position->enPassantSquare) {
            addCaptureMove(position, list, from, to, BLACK_PAWN, WHITE_PAWN, 0, FLAG_EP);
        }
        else {
            int captured = getBlackPieceAt(position, 1ULL << to);
            addCaptureMove(position, list, from, to, captured, WHITE_PAWN, 0, 0);
        }
    }
    while (rightAttacks) {
        to = peekBit(rightAttacks); popBit(rightAttacks);
        int from = to - 9;
        int captured = getBlackPieceAt(position, 1ULL << to);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_QUEEN, 0);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_KNIGHT, 0);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_BISHOP, 0);
        addCaptureMove(position, list, from, to, captured, WHITE_PAWN, WHITE_ROOK, 0);
    }
}
static void generateBlackPawnMoves(const Board* position, MoveList* list) {
    U64 advance1 = (position->pieceBB[BLACK_PAWN] >> 8)& position->emptyBB;
    U64 advance2 = ((advance1 & RANK_6_MASK) >> 8)& position->emptyBB;
    int to;
    while (advance1 & RANK_1_MASK) {
        to = peekBit(advance1 & RANK_1_MASK); popBit(advance1);
        int from = to + 8;
        addQuietMove(position, list, from, to, BLACK_PAWN, BLACK_QUEEN, 0);
        addQuietMove(position, list, from, to, BLACK_PAWN, BLACK_KNIGHT, 0);
        addQuietMove(position, list, from, to, BLACK_PAWN, BLACK_BISHOP, 0);
        addQuietMove(position, list, from, to, BLACK_PAWN, BLACK_ROOK, 0);
    }
    while (advance1) {
        to = peekBit(advance1); popBit(advance1);
        addQuietMove(position, list, to + 8, to, BLACK_PAWN, 0, 0);
    }
    while (advance2) {
        to = peekBit(advance2); popBit(advance2);
        addQuietMove(position, list, to + 16, to, BLACK_PAWN, 0, FLAG_PS);
    }

    U64 leftAttacks = (position->pieceBB[BLACK_PAWN] >> 7)& NOT_FILE_A_MASK& (position->pieceBB[WHITE] | setMask[position->enPassantSquare]);
    while (leftAttacks & RANK_1_MASK) {
        to = peekBit(leftAttacks & RANK_1_MASK); popBit(leftAttacks);
        int from = to + 7;
        int captured = getWhitePieceAt(position, 1ULL << to);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_QUEEN, 0);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_KNIGHT, 0);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_BISHOP, 0);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_ROOK, 0);
    }
    while (leftAttacks) {
        to = peekBit(leftAttacks); popBit(leftAttacks);
        int from = to + 7;
        if (to == position->enPassantSquare) {
            addCaptureMove(position, list, from, to, WHITE_PAWN, BLACK_PAWN, 0, FLAG_EP);
        }
        else {
            int captured = getWhitePieceAt(position, 1ULL << to);
            addCaptureMove(position, list, from, to, captured, BLACK_PAWN, 0, 0);
        }
    }

    U64 rightAttacks = (position->pieceBB[BLACK_PAWN] >> 9)& NOT_FILE_H_MASK& (position->pieceBB[WHITE] | setMask[position->enPassantSquare]);
    while (rightAttacks & RANK_1_MASK) {
        to = peekBit(rightAttacks & RANK_1_MASK); popBit(rightAttacks);
        int from = to + 9;
        int captured = getWhitePieceAt(position, 1ULL << to);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_QUEEN, 0);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_KNIGHT, 0);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_BISHOP, 0);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_ROOK, 0);
    }
    while (rightAttacks) {
        to = peekBit(rightAttacks); popBit(rightAttacks);
        int from = to + 9;
        if (to == position->enPassantSquare) {
            addCaptureMove(position, list, from, to, WHITE_PAWN, BLACK_PAWN, 0, FLAG_EP);
        }
        else {
            int captured = getWhitePieceAt(position, 1ULL << to);
            addCaptureMove(position, list, from, to, captured, BLACK_PAWN, 0, 0);
        }
    }
}
static void generateBlackPawnCaptures(const Board* position, MoveList* list) {
    int to;
    U64 leftAttacks = (position->pieceBB[BLACK_PAWN] >> 7)& NOT_FILE_A_MASK& (position->pieceBB[WHITE] | setMask[position->enPassantSquare]);
    while (leftAttacks & RANK_1_MASK) {
        to = peekBit(leftAttacks & RANK_1_MASK); popBit(leftAttacks);
        int from = to + 7;
        int captured = getWhitePieceAt(position, 1ULL << to);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_QUEEN, 0);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_KNIGHT, 0);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_BISHOP, 0);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_ROOK, 0);
    }
    while (leftAttacks) {
        to = peekBit(leftAttacks); popBit(leftAttacks);
        int from = to + 7;
        if (to == position->enPassantSquare) {
            addCaptureMove(position, list, from, to, WHITE_PAWN, BLACK_PAWN, 0, FLAG_EP);
        }
        else {
            int captured = getWhitePieceAt(position, 1ULL << to);
            addCaptureMove(position, list, from, to, captured, BLACK_PAWN, 0, 0);
        }
    }

    U64 rightAttacks = (position->pieceBB[BLACK_PAWN] >> 9)& NOT_FILE_H_MASK& (position->pieceBB[WHITE] | setMask[position->enPassantSquare]);
    while (rightAttacks & RANK_1_MASK) {
        to = peekBit(rightAttacks & RANK_1_MASK); popBit(rightAttacks);
        int from = to + 9;
        int captured = getWhitePieceAt(position, 1ULL << to);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_QUEEN, 0);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_KNIGHT, 0);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_BISHOP, 0);
        addCaptureMove(position, list, from, to, captured, BLACK_PAWN, BLACK_ROOK, 0);
    }
    while (rightAttacks) {
        to = peekBit(rightAttacks); popBit(rightAttacks);
        int from = to + 9;
        if (to == position->enPassantSquare) {
            addCaptureMove(position, list, from, to, WHITE_PAWN, BLACK_PAWN, 0, FLAG_EP);
        }
        else {
            int captured = getWhitePieceAt(position, 1ULL << to);
            addCaptureMove(position, list, from, to, captured, BLACK_PAWN, 0, 0);
        }
    }
}

static void generateWhiteKingMoves(const Board* position, MoveList* list) {
    int from = peekBit(position->pieceBB[WHITE_KING]);
    U64 moves = kingAttacks[from] & position->emptyBB;
    int to;
    while (moves) {
        to = peekBit(moves); popBit(moves);
        addQuietMove(position, list, from, to, WHITE_KING, 0, 0);
    }
    U64 attacks = kingAttacks[from] & position->pieceBB[BLACK];
    while (attacks) {
        to = peekBit(attacks); popBit(attacks);
        int captured = getBlackPieceAt(position, 1ULL << to);
        addCaptureMove(position, list, from, to, captured, WHITE_KING, 0, 0);
    }
    if ((position->castlePermissions & WHITE_KING_CASTLE) && ((position->emptyBB & 0x0000000000000060) == 0x0000000000000060)) {
        if (!attackedByBlack(position, E1) && !attackedByBlack(position, F1)) {
            addQuietMove(position, list, E1, G1, WHITE_KING, 0, FLAG_CA);
        }
    }
    if ((position->castlePermissions & WHITE_QUEEN_CASTLE) && ((position->emptyBB & 0x000000000000000E) == 0x000000000000000E)) {
        if (!attackedByBlack(position, E1) && !attackedByBlack(position, D1)) {
            addQuietMove(position, list, E1, C1, WHITE_KING, 0, FLAG_CA);
        }
    }
}
static void generateWhiteKingCaptures(const Board* position, MoveList* list) {
    int from = peekBit(position->pieceBB[WHITE_KING]);
    U64 attacks = kingAttacks[from] & position->pieceBB[BLACK];
    while (attacks) {
        int to = peekBit(attacks); popBit(attacks);
        int captured = getBlackPieceAt(position, 1ULL << to);
        addCaptureMove(position, list, from, to, captured, WHITE_KING, 0, 0);
    }
}
static void generateBlackKingMoves(const Board* position, MoveList* list) {
    int from = peekBit(position->pieceBB[BLACK_KING]);
    U64 moves = kingAttacks[from] & position->emptyBB;
    int to;
    while (moves) {
        to = peekBit(moves); popBit(moves);
        addQuietMove(position, list, from, to, BLACK_KING, 0, 0);
    }
    U64 attacks = kingAttacks[from] & position->pieceBB[WHITE];
    while (attacks) {
        to = peekBit(attacks); popBit(attacks);
        int captured = getWhitePieceAt(position, 1ULL << to);
        addCaptureMove(position, list, from, to, captured, BLACK_KING, 0, 0);
    }
    if ((position->castlePermissions & BLACK_KING_CASTLE) && ((position->emptyBB & 0x6000000000000000) == 0x6000000000000000)) {
        if (!attackedByWhite(position, E8) && !attackedByWhite(position, F8)) {
            addQuietMove(position, list, E8, G8, BLACK_KING, 0, FLAG_CA);
        }
    }
    if ((position->castlePermissions & BLACK_QUEEN_CASTLE) && ((position->emptyBB & 0x0E00000000000000) == 0x0E00000000000000)) {
        if (!attackedByWhite(position, E8) && !attackedByWhite(position, D8)) {
            addQuietMove(position, list, E8, C8, BLACK_KING, 0, FLAG_CA);
        }
    }
}
static void generateBlackKingCaptures(const Board* position, MoveList* list) {
    int from = peekBit(position->pieceBB[BLACK_KING]);
    U64 attacks = kingAttacks[from] & position->pieceBB[WHITE];
    while (attacks) {
        int to = peekBit(attacks); popBit(attacks);
        int captured = getWhitePieceAt(position, 1ULL << to);
        addCaptureMove(position, list, from, to, captured, BLACK_KING, 0, 0);
    }
}

void generateMoves(const Board* position, MoveList* list) {
    list->count = 0;
    if (position->side == WHITE) {
        generateWhitePawnMoves(position, list);
        generateWhiteKingMoves(position, list);
        generatorRoutineWhite(position, WHITE_KNIGHT, list);
        generatorRoutineWhite(position, WHITE_BISHOP, list);
        generatorRoutineWhite(position, WHITE_ROOK, list);
        generatorRoutineWhite(position, WHITE_QUEEN, list);
    }
    else {
        generateBlackPawnMoves(position, list);
        generateBlackKingMoves(position, list);
        generatorRoutineBlack(position, BLACK_KNIGHT, list);
        generatorRoutineBlack(position, BLACK_BISHOP, list);
        generatorRoutineBlack(position, BLACK_ROOK, list);
        generatorRoutineBlack(position, BLACK_QUEEN, list);
    }
}
void generateCaptures(const Board* position, MoveList* list) {
    list->count = 0;
    if (position->side == WHITE) {
        generateWhitePawnCaptures(position, list);
        generateWhiteKingCaptures(position, list);
        generatorRoutineWhiteCaptures(position, WHITE_KNIGHT, list);
        generatorRoutineWhiteCaptures(position, WHITE_BISHOP, list);
        generatorRoutineWhiteCaptures(position, WHITE_ROOK, list);
        generatorRoutineWhiteCaptures(position, WHITE_QUEEN, list);
    }
    else {
        generateBlackPawnCaptures(position, list);
        generateBlackKingCaptures(position, list);
        generatorRoutineBlackCaptures(position, BLACK_KNIGHT, list);
        generatorRoutineBlackCaptures(position, BLACK_BISHOP, list);
        generatorRoutineBlackCaptures(position, BLACK_ROOK, list);
        generatorRoutineBlackCaptures(position, BLACK_QUEEN, list);
    }
}

char* PrSq(const int sq);
char* PrMove(const int move);

void debugMove(int move) {
    string names[14] = { "none","error","white pawn","white knight","white bishop","white rook","white queen","white king","black pawn","black knight","black bishop","black rook","black queen","black king" };

    int f = from(move);
    int t = to(move);
    int moving = moving(move);
    int captured = captured(move);
    int promoted = promoted(move);

    printf("move %s (%d) details:\n", PrMove(move), move);
    printf("from %d, to %d\n", f, t);
    char* pf = PrSq(f);
    printf("pf %s\n", pf);
    char* pt = PrSq(t);
    printf("pt %s\n", pt);
    printf("moving: %s\n", names[moving]);
    printf("captured: %s\n", names[captured]);
    printf("promoted: %s\n", names[promoted]);
    printf("flags: \n");
    if (isCapture(move)) printf("capture \n");
    if (isPromote(move)) printf("promote \n");
    if (isCastle(move)) printf("castle \n");
    if (isPawnStart(move)) printf("pawn_start \n");
    if (isEnPassant(move)) printf("en_passant \n");
}

char* PrSq(const int sq) {

    static char SqStr[3];

    int file = fileArray[sq];
    int rank = rankArray[sq];

    sprintf(SqStr, "%c%c", ('a' + file), ('1' + rank));

    return SqStr;

}

char* PrMove(const int move) {

    static char MvStr[6];

    int ff = fileArray[from(move)];
    int rf = rankArray[from(move)];
    int ft = fileArray[to(move)];
    int rt = rankArray[to(move)];

    int promoted = isPromote(move);

    if (promoted) {
        char pchar = 'q';
        int promotedPiece = promoted(move);
        if ((promotedPiece == WHITE_KNIGHT) || (promotedPiece == BLACK_KNIGHT)) {
            pchar = 'n';
        }
        else if ((promotedPiece == WHITE_ROOK) || (promotedPiece == BLACK_ROOK)) {
            pchar = 'r';
        }
        else if ((promotedPiece == WHITE_BISHOP) || (promotedPiece == BLACK_BISHOP)) {
            pchar = 'b';
        }
        sprintf(MvStr, "%c%c%c%c%c", ('a' + ff), ('1' + rf), ('a' + ft), ('1' + rt), pchar);
    }
    else {
        sprintf(MvStr, "%c%c%c%c", ('a' + ff), ('1' + rf), ('a' + ft), ('1' + rt));
    }

    return MvStr;
}

void printMoveList(const MoveList list) {
    int index = 0;
    int score = 0;
    int move = 0;
    printf("MoveList:\n");

    for (index = 0; index < list.count; ++index) {

        move = list.moves[index].move;
        score = list.moves[index].score;

        printf("Move:%d > %s (score:%d)\n", index + 1, PrMove(move), score);
    }
    printf("MoveList Total %d Moves:\n\n", list.count);
}

int moveExists(Board* position, const int move) {
    MoveList list[1];
    generateMoves(position, list);

    for (int i = 0; i < list->count; i++) {
        if (list->moves[i].move == move) {
            int validMove = makeMove(position, list->moves[i].move);
            unmakeMove(position);
            return validMove;
        }
    }
    return 0;
}

//make move

const int castlePermissionsBoard[64] = {
    13, 15, 15, 15, 12, 15, 15, 14,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15,
    7, 15, 15, 15,  3, 15, 15, 11
};

#define HASH_PIECE(piece, square) (position->posKey ^= pieceKeys[(piece)][(square)])
#define HASH_CASTLE (position->posKey ^= castleKeys[position->castlePermissions])
#define HASH_SIDE (position->posKey ^= sideKey)
#define HASH_EN_PASSANT (position->posKey ^= enPassantKeys[position->enPassantSquare])

void movePiece(Board* position, int piece, int from, int to) {
    position->pieceBB[piece] ^= ((1ULL << from) | (1ULL << to));
    HASH_PIECE(piece, from);
    HASH_PIECE(piece, to);
    position->pieceBB[position->side] ^= ((1ULL << from) | (1ULL << to));
}

void flipPiece(Board* position, int piece, int square, int side) {
    position->pieceBB[piece] ^= (1ULL << square);
    HASH_PIECE(piece, square);
    position->pieceBB[side] ^= (1ULL << square);
}

void unmakeMove(Board* position) {
    position->histPly--;
    position->ply--;

    int move = position->history[position->histPly].move;
    position->castlePermissions = position->history[position->histPly].castlePermissions;
    position->fiftyMoveCounter = position->history[position->histPly].fiftyMoveCounter;
    position->enPassantSquare = position->history[position->histPly].enPassantSquare;
    position->posKey = position->history[position->histPly].posKey;

    position->side ^= 1;

    int from = from(move);
    int to = to(move);
    int movingPiece = moving(move);

    if (isPromote(move)) {
        int promotedPiece = promoted(move);
        position->pieceBB[promotedPiece] ^= (1ULL << to);
        position->pieceBB[movingPiece] ^= (1ULL << to);
    }

    position->pieceBB[movingPiece] ^= ((1ULL << from) | (1ULL << to));
    position->pieceBB[position->side] ^= ((1ULL << from) | (1ULL << to));

    if (isCapture(move)) {
        int target = to;
        if (isEnPassant(move)) {
            if (position->side == WHITE) target = to - 8;
            else target = to + 8;
        }
        int capturedPiece = captured(move);
        position->pieceBB[capturedPiece] ^= (1ULL << target);
        position->pieceBB[position->side ^ 1] ^= (1ULL << target);
    }
    else if (isCastle(move)) {
        switch (to) {
        case G1: position->pieceBB[WHITE_ROOK] ^= ((1ULL << H1) | (1ULL << F1));
            position->pieceBB[WHITE] ^= ((1ULL << H1) | (1ULL << F1));
            break;
        case C1: position->pieceBB[WHITE_ROOK] ^= ((1ULL << A1) | (1ULL << D1));
            position->pieceBB[WHITE] ^= ((1ULL << A1) | (1ULL << D1));
            break;
        case G8: position->pieceBB[BLACK_ROOK] ^= ((1ULL << H8) | (1ULL << F8));
            position->pieceBB[BLACK] ^= ((1ULL << H8) | (1ULL << F8));
            break;
        case C8: position->pieceBB[BLACK_ROOK] ^= ((1ULL << A8) | (1ULL << D8));
            position->pieceBB[BLACK] ^= ((1ULL << A8) | (1ULL << D8));
            break;
        }
    }

    position->occupiedBB = position->pieceBB[WHITE] | position->pieceBB[BLACK];
    position->emptyBB = ~position->occupiedBB;
}

int makeMove(Board* position, const int move) {
    position->history[position->histPly].move = move;
    position->history[position->histPly].castlePermissions = position->castlePermissions;
    position->history[position->histPly].fiftyMoveCounter = position->fiftyMoveCounter;
    position->history[position->histPly].enPassantSquare = position->enPassantSquare;
    position->history[position->histPly].posKey = position->posKey;
    position->histPly++;
    position->ply++;

    int from = from(move);
    int to = to(move);
    int movingPiece = moving(move);

    position->fiftyMoveCounter++;

    if (position->enPassantSquare < 64) {
        HASH_EN_PASSANT;
        position->enPassantSquare = 64;
    }

    HASH_CASTLE;
    position->castlePermissions &= castlePermissionsBoard[from] & castlePermissionsBoard[to];
    HASH_CASTLE;

    movePiece(position, movingPiece, from, to);

    if (isCapture(move)) {

        position->fiftyMoveCounter = 0;
        int capturedPiece = captured(move);

        int target = to;
        if (isEnPassant(move)) {
            if (position->side == WHITE) target = to - 8;
            else target = to + 8;
        }
        else if (isPromote(move)) {

            int promotedPiece = promoted(move);
            flipPiece(position, movingPiece, to, position->side);
            flipPiece(position, promotedPiece, to, position->side);
        }
        flipPiece(position, capturedPiece, target, position->side ^ 1);
    }
    else if (isCastle(move)) {

        switch (to) {
        case G1: movePiece(position, WHITE_ROOK, H1, F1); break;
        case C1: movePiece(position, WHITE_ROOK, A1, D1); break;
        case G8: movePiece(position, BLACK_ROOK, H8, F8); break;
        case C8: movePiece(position, BLACK_ROOK, A8, D8); break;
        }
    }
    else if (isPawnStart(move)) {

        position->fiftyMoveCounter = 0;
        position->enPassantSquare = (from + to) / 2;
        HASH_EN_PASSANT;
    }
    else if (isPromote(move)) {

        position->fiftyMoveCounter = 0;
        int promotedPiece = promoted(move);
        flipPiece(position, movingPiece, to, position->side);
        flipPiece(position, promotedPiece, to, position->side);
    }
    else if (movingPiece == WHITE_PAWN || movingPiece == BLACK_PAWN) {
        position->fiftyMoveCounter = 0;
    }

    position->occupiedBB = position->pieceBB[WHITE] | position->pieceBB[BLACK];
    position->emptyBB = ~position->occupiedBB;

    position->side ^= 1;
    HASH_SIDE;

    if (underCheck(position, position->side ^ 1)) {
        unmakeMove(position);
        return 0;
    }
    return 1;
}

void MakeNullMove(Board* position) {

    position->ply++;
    position->history[position->histPly].posKey = position->posKey;

    if (position->enPassantSquare != 64) HASH_EN_PASSANT;

    position->history[position->histPly].move = NOMOVE;
    position->history[position->histPly].fiftyMoveCounter = position->fiftyMoveCounter;
    position->history[position->histPly].enPassantSquare = position->enPassantSquare;
    position->history[position->histPly].castlePermissions = position->castlePermissions;
    position->enPassantSquare = 64;

    position->side ^= 1;
    position->histPly++;
    HASH_SIDE;

    return;
}

void TakeNullMove(Board* position) {

    position->histPly--;
    position->ply--;

    if (position->enPassantSquare != 64) HASH_EN_PASSANT;

    position->castlePermissions = position->history[position->histPly].castlePermissions;
    position->fiftyMoveCounter = position->history[position->histPly].fiftyMoveCounter;
    position->enPassantSquare = position->history[position->histPly].enPassantSquare;

    if (position->enPassantSquare != 64) HASH_EN_PASSANT;
    position->side ^= 1;
    HASH_SIDE;
}

//hash table

const int pvSize = 0x100000 * 64;

int probePV(const Board *position) {
    int index = position->posKey % position->HashTable->numEntries;

    if (position->HashTable->pTable[index].posKey == position->posKey) {
        return position->HashTable->pTable[index].move;
    }
    return NOMOVE;
}

int getPV(Board* position, const int depth) {
    int move = probePV(position);
    int count = 0;

    while (move != NOMOVE && count < depth) {
        if (moveExists(position, move)) {
            makeMove(position, move);
            position->pvArray[count++] = move;
        }
        else {
            break;
        }
        move = probePV(position);
    }

    while (position->ply > 0) {
        unmakeMove(position);
    }

    return count;
}

void clearHashTable(HashTable* table) {

    HashEntry *tableEntry;

    for (tableEntry = table->pTable; tableEntry < table->pTable + table->numEntries; tableEntry++) {
        tableEntry->posKey = 0ULL;
        tableEntry->move = NOMOVE;
        tableEntry->depth = 0;
        tableEntry->score = 0;
        tableEntry->flags = 0;
    }
    table->newWrite = 0;
}

void initHashTable(HashTable* table, int MB) {

    int HashSize = 0x100000 * MB;
    table->numEntries = HashSize / sizeof(HashEntry);
    table->numEntries -= 2;

    if (table->pTable != NULL) {
        free(table->pTable);
    }

    table->pTable = (HashEntry*)malloc(table->numEntries * sizeof(HashEntry));
    if (table->pTable == NULL) {
        printf("Hash Allocation Failed, trying %dMB...\n", MB / 2);
        initHashTable(table, MB / 2);
    }
    else {
        clearHashTable(table);
        printf("HashTable init complete with %d entries\n", table->numEntries);
    }

}

void storeHashEntry(Board *position, const int move, int score, const int depth, const int flags) {
    int index = position->posKey % position->HashTable->numEntries;

    if (position->HashTable->pTable[index].posKey == 0) {
        position->HashTable->newWrite++;
    }
    else {
        position->HashTable->overWrite++;
    }

    if (score > ISMATE) score += position->ply;
    else if (score < -ISMATE) score -= position->ply;

    position->HashTable->pTable[index].posKey = position->posKey;
    position->HashTable->pTable[index].move = move;
    position->HashTable->pTable[index].score = score;
    position->HashTable->pTable[index].depth = depth;
    position->HashTable->pTable[index].flags = flags;
}

int probeHashEntry(Board* position, int *move, int *score, int alpha, int beta, int depth) {
    int index = position->posKey % position->HashTable->numEntries;
    
    if (position->HashTable->pTable[index].posKey == position->posKey) {
        *move = position->HashTable->pTable[index].move;
        if (position->HashTable->pTable[index].depth >= depth) {
            position->HashTable->hit++;

            *score = position->HashTable->pTable[index].score;
            if (*score > ISMATE) *score -= position->ply;
            else if (*score < -ISMATE) *score += position->ply;

            switch (position->HashTable->pTable[index].flags) {

            case FLAG_ALPHA: if (*score <= alpha) {
                *score = alpha;
                return 1;
            }
                        break;
            case FLAG_BETA: if (*score >= beta) {
                *score = beta;
                return 1;
            }
                       break;
            case FLAG_EXACT:
                return 1;
                break;
            default:
                break;
            }
        }
    }

    return 0;
}

//evaluate

const int evaluatePieceBoards(const int valueBoard[64], const int pieceValue, U64 whitePieces, U64 blackPieces) {
    int score = 0;
    while (whitePieces) {
        int index = peekBit(whitePieces); popBit(whitePieces);
        score += pieceValue + valueBoard[index];
    }
    while (blackPieces) {
        int index = peekBit(blackPieces); popBit(blackPieces);
        score -= pieceValue + valueBoard[mirror[index]];
    }
    return score;
}

int evaluatePosition(const Board* position) {
    int earlyEval = (bitCount(position->pieceBB[WHITE_PAWN])/*bitCount(wP)*/ - bitCount(position->pieceBB[BLACK_PAWN])/*bitCount(bP)*/) * pieceValue[WHITE_PAWN];
    int lateEval = earlyEval;

    earlyEval += evaluatePieceBoards(pawnTableEarly, 0, position->pieceBB[WHITE_PAWN], position->pieceBB[BLACK_PAWN]);
    earlyEval += evaluatePieceBoards(knightTableEarly, pieceValue[WHITE_KNIGHT], position->pieceBB[WHITE_KNIGHT], position->pieceBB[BLACK_KNIGHT]);
    earlyEval += evaluatePieceBoards(bishopTableEarly, pieceValue[WHITE_BISHOP], position->pieceBB[WHITE_BISHOP], position->pieceBB[BLACK_BISHOP]);
    earlyEval += evaluatePieceBoards(rookTableEarly, pieceValue[WHITE_ROOK], position->pieceBB[WHITE_ROOK], position->pieceBB[BLACK_ROOK]);
    earlyEval += evaluatePieceBoards(queenTableEarly, pieceValue[WHITE_QUEEN], position->pieceBB[WHITE_QUEEN], position->pieceBB[BLACK_QUEEN]);
    earlyEval += evaluatePieceBoards(kingTableEarly, 0, position->pieceBB[WHITE_KING], position->pieceBB[BLACK_KING]);

    lateEval += evaluatePieceBoards(pawnTableLate, 0, position->pieceBB[WHITE_PAWN], position->pieceBB[BLACK_PAWN]);
    lateEval += evaluatePieceBoards(knightTableLate, pieceValue[WHITE_KNIGHT], position->pieceBB[WHITE_KNIGHT], position->pieceBB[BLACK_KNIGHT]);
    lateEval += evaluatePieceBoards(bishopTableLate, pieceValue[WHITE_BISHOP], position->pieceBB[WHITE_BISHOP], position->pieceBB[BLACK_BISHOP]);
    lateEval += evaluatePieceBoards(rookTableLate, pieceValue[WHITE_ROOK], position->pieceBB[WHITE_ROOK], position->pieceBB[BLACK_ROOK]);
    lateEval += evaluatePieceBoards(queenTableLate, pieceValue[WHITE_QUEEN], position->pieceBB[WHITE_QUEEN], position->pieceBB[BLACK_QUEEN]);
    lateEval += evaluatePieceBoards(kingTableLate, 0, position->pieceBB[WHITE_KING], position->pieceBB[BLACK_KING]);

    double gameProgress = (bitCount(position->occupiedBB)) / 32.0;
    int evaluation = (int)((gameProgress * earlyEval) + ((1 - gameProgress) * lateEval));

    if (position->side == WHITE)
    {
        return evaluation;
    }
    else
    {
        return -evaluation;
    }
}

//board functions

U64 generateHashKey(const Board* position) {
    U64 key = 0ULL;
    for (int i = 2; i < 14; i++) {
        U64 bb = position->pieceBB[i];
        for (int j = 0; j < 64; j++) {
            if (bb & setMask[j]) key ^= pieceKeys[i][j];
        }
    }
    if (position->side == WHITE) key ^= sideKey;
    if (position->enPassantSquare != 64) key ^= enPassantKeys[position->enPassantSquare];
    key ^= castleKeys[position->castlePermissions];
    return key;
}

void initBoard(Board* position, char* fen)
{
    for (int i = 0; i < 14; i++)
    {
        position->pieceBB[i] = 0ULL;
    }

    position->ply = 0;
    position->histPly = 0;

    position->enPassantSquare = 64;
    position->fiftyMoveCounter = 0;

    position->posKey = 0ULL;

    int rank = RANK_8;
    int file = FILE_A;
    int piece = -1;
    int count = 0;
    int i = 0;

    while ((rank >= RANK_1) && *fen)
    {
        count = 1;
        switch (*fen) 
        {
        case 'r': piece = BLACK_ROOK; break;
        case 'n': piece = BLACK_KNIGHT; break;
        case 'b': piece = BLACK_BISHOP; break;
        case 'q': piece = BLACK_QUEEN; break;
        case 'k': piece = BLACK_KING; break;
        case 'p': piece = BLACK_PAWN; break;
        case 'R': piece = WHITE_ROOK; break;
        case 'N': piece = WHITE_KNIGHT; break;
        case 'B': piece = WHITE_BISHOP; break;
        case 'Q': piece = WHITE_QUEEN; break;
        case 'K': piece = WHITE_KING; break;
        case 'P': piece = WHITE_PAWN; break;

        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
            piece = -1;
            count = *fen - '0';
            break;

        case '/':
        case ' ':
            rank--;
            file = FILE_A;
            fen++;
            continue;

        default:
            printf("FEN error \n");
            fflush(stdout);
            return;
        }

        for (i = 0; i < count; i++)
        {
            if (piece > 0)
            {
                setBit(position->pieceBB[piece], getSquare(file, rank));
            }
            file++;
        }
        fen++;
    }

    position->side = ((*fen == 'w') ? WHITE : BLACK);

    fen += 2;

    for (int i = 0; i < 4; i++)
    {
        if (*fen == ' ')
        {
            break;
        }
        switch (*fen)
        {
        case 'K': position->castlePermissions |= WHITE_KING_CASTLE; break;
        case 'Q': position->castlePermissions |= WHITE_QUEEN_CASTLE; break;
        case 'k': position->castlePermissions |= BLACK_KING_CASTLE; break;
        case 'q': position->castlePermissions |= BLACK_QUEEN_CASTLE; break;
        default: break;
        }
        fen++;
    }
    fen++;

    if (*fen != '-')
    {
        file = fen[0] - 'a';
        rank = fen[1] - '1';

        position->enPassantSquare = getSquare(file, rank);
    }

    position->pieceBB[WHITE] = 0ULL | position->pieceBB[WHITE_ROOK] | position->pieceBB[WHITE_KNIGHT] | position->pieceBB[WHITE_BISHOP] | position->pieceBB[WHITE_QUEEN] | position->pieceBB[WHITE_KING] | position->pieceBB[WHITE_PAWN];
    position->pieceBB[BLACK] = 0ULL | position->pieceBB[BLACK_ROOK] | position->pieceBB[BLACK_KNIGHT] | position->pieceBB[BLACK_BISHOP] | position->pieceBB[BLACK_QUEEN] | position->pieceBB[BLACK_KING] | position->pieceBB[BLACK_PAWN];
    position->occupiedBB = 0ULL | position->pieceBB[WHITE] | position->pieceBB[BLACK];
    position->emptyBB = ~position->occupiedBB;

    position->posKey = generateHashKey(position);

}

int checkHashKey(const Board* position) {
    int valid;
    if (generateHashKey(position) == position->posKey)
    {
        valid = 1;
    }
    else
    {
        valid = 0;
    }
    if (!valid) {
        for (int i = 0; i < position->histPly; i++) {
            debugMove(position->history[i].move);
        }
    }
    return valid;
}

//io

int parseMove(const Board* position, char* ptrChar) {
    int from = getSquare(ptrChar[0] - 'a', ptrChar[1] - '1');
    int to = getSquare(ptrChar[2] - 'a', ptrChar[3] - '1');

    MoveList list[1];
    generateMoves(position, list);

    for (int i = 0; i < list->count; ++i) {
        int moveTest = list->moves[i].move;
        if (from(moveTest) == from && to(moveTest) == to) {
            if (isPromote(moveTest)) {
                char promote = ptrChar[4];
                int promotedPiece = promoted(moveTest);
                switch (promote) {
                case 'b': if (promotedPiece == WHITE_BISHOP || promotedPiece == BLACK_BISHOP) return moveTest; break;
                case 'n': if (promotedPiece == WHITE_KNIGHT || promotedPiece == BLACK_KNIGHT) return moveTest; break;
                case 'r': if (promotedPiece == WHITE_ROOK || promotedPiece == BLACK_ROOK) return moveTest; break;
                case 'q': if (promotedPiece == WHITE_QUEEN || promotedPiece == BLACK_QUEEN) return moveTest; break;
                default: break;
                }
                continue;
            }
            return moveTest;
        }
    }
    return NOMOVE;
}

void printBoard(Board* position) {
    char ids[14] = { '!','!','P','N','B','R','Q','K','p','n','b','r','q','k' };

    int rank;

    for (int r = RANK_8; r >= RANK_1; r--) {
        printf("%d  ", r + 1);
        for (int f = FILE_A; f <= FILE_H; f++) {
            int square = getSquare(f, r);
            int hit = 0;
            for (int piece = 2; piece < 14; piece++) {
                if (position->pieceBB[piece] & setMask[square]) {
                    hit = 1;
                    printf(" %c ", ids[piece]);
                    break;
                }
            }
            if (!hit) printf(" . ");
            
        }
        printf("\n");
    }
    printf("    a  b  c  d  e  f  g  h ");
    printf("\nside: ");
    if (position->side == WHITE) printf("WHITE");
    else printf("BLACK");
    printf("\ncastle permissions: ");
    int castlePermissions = position->castlePermissions;
    if (castlePermissions & WHITE_KING_CASTLE) printf("K");
    if (castlePermissions & WHITE_QUEEN_CASTLE) printf("Q");
    if (castlePermissions & BLACK_KING_CASTLE) printf("k");
    if (castlePermissions & BLACK_QUEEN_CASTLE) printf("q");
    printf("\nen passant: %d", position->enPassantSquare);
    printf("\nPosKey:%llX\n", position->posKey);
}

//misc

int InputWaiting()
{
    static int init = 0, pipe;
    static HANDLE inh;
    DWORD dw;

    if (!init) {
        init = 1;
        inh = GetStdHandle(STD_INPUT_HANDLE);
        pipe = !GetConsoleMode(inh, &dw);
        if (!pipe) {
            SetConsoleMode(inh, dw & ~(ENABLE_MOUSE_INPUT | ENABLE_WINDOW_INPUT));
            FlushConsoleInputBuffer(inh);
        }
    }
    if (pipe) {
        if (!PeekNamedPipe(inh, NULL, 0, NULL, &dw, NULL)) return 1;
        return dw;
    }
    else {
        GetNumberOfConsoleInputEvents(inh, &dw);
        return dw <= 1 ? 0 : dw;
    }
}

void readInput(SearchInfo* info) {
    int             bytes;
    char            input[256] = "", * endc;

    if (InputWaiting()) {
        info->stopped = 1;
        do {
            bytes = read(fileno(stdin), input, 256);
        } while (bytes < 0);
        endc = strchr(input, '\n');
        if (endc) *endc = 0;

        if (strlen(input) > 0) {
            if (!strncmp(input, "quit", 4)) {
                info->quit = 1;
            }
        }
        return;
    }
}

//perft

long leafNodes;

void perft(Board* position, const int depth) {
    if (depth == 0) {
        leafNodes++;
        return;
    }

    MoveList list[1];
    generateMoves(position, list);
    for (int i = 0; i < list->count; i++) {
        int move = list->moves[i].move;
        if (!makeMove(position, move)) continue;
        perft(position, depth - 1);
        unmakeMove(position);
    }

    return;
}

void PerftTest(int depth, Board* position) {

    printBoard(position);
    printf("\nStarting Test To Depth:%d\n", depth);
    leafNodes = 0;
    int start = GetTickCount();
    MoveList list[1];
    generateMoves(position, list);

    int move;
    int MoveNum = 0;
    for (MoveNum = 0; MoveNum < list->count; ++MoveNum) {
        move = list->moves[MoveNum].move;
        if (!makeMove(position, move)) {
            continue;
        }
        long cumnodes = leafNodes;
        perft(position, depth - 1);
        unmakeMove(position);
        long oldnodes = leafNodes - cumnodes;
        printf("move %d : %s : %ld\n", MoveNum + 1, PrMove(move), oldnodes);
    }

    printf("\nTest Complete : %ld nodes visited in %dms\n", leafNodes, GetTickCount() - start);

    return;
}

//search

static void checkUp(SearchInfo *info) {
    // check for interrupt from GUI
    if (info->timeSet == 1 && GetTickCount() > (info->endTime)) {
        info->stopped = 1;
    }
    readInput(info);
}

static void clearForSearch(Board* position, SearchInfo *info) {
    for (int i = 0; i < 14; i++) {
        for (int j = 0; j < 64; j++) {
            position->searchHistory[i][j] = 0;
        }
    }
    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < MAX_DEPTH; j++) {
            position->searchKillers[i][j] = 0;
        }
    }

    position->HashTable->overWrite = 0;
    position->HashTable->hit = 0;
    position->HashTable->cut = 0;
    position->ply = 0;

    info->stopped = 0;
    info->nodes = 0;
    info->failHigh = 0;
    info->failHighFirst = 0;
}

static int isRepetition(Board* position) {
    for (int i = position->histPly - position->fiftyMoveCounter; i < position->histPly - 1; i++) {
        if (position->posKey == position->history[i].posKey) {
            return 1;
        }
    }
    return 0;
}

static void pickNextMove(int moveNum, MoveList* list) {
    int bestScore = 0;
    int bestNum = moveNum;

    for (int i = moveNum; i < list->count; i++) {
        if (list->moves[i].score > bestScore) {
            bestScore = list->moves[i].score;
            bestNum = i;
        }
    }

    Move temp = list->moves[moveNum];
    list->moves[moveNum] = list->moves[bestNum];
    list->moves[bestNum] = temp;
}

int MAX(int num1, int num2)
{
   if (num1 > num2)
   {
       return num1;
   }
   else
   {
       return num2;
   }
}

int MIN(int num1, int num2)
{
    if (num1 > num2)
    {
        return num2;
    }
    else
    {
        return num1;
    }
}

static int quiescence(Board* position, SearchInfo* info, int alpha, int beta) {
    
    if ((info->nodes & 2047) == 0) {
        checkUp(info);
    }

    info->nodes++;

    if (isRepetition(position) || position->fiftyMoveCounter >= 100) {
        return 0;
    }
    if (position->ply >= MAX_DEPTH) {
        return evaluatePosition(position);
    }

    // Mate Distance Pruning
    alpha = MAX(alpha, -INFINITE + position->ply);
    beta = MIN(beta, INFINITE - position->ply);
    if (alpha >= beta) {
        return alpha;
    }

    int score = evaluatePosition(position);

    if (score >= beta) {
        return beta;
    }

    if (score > alpha) {
        alpha = score;
    }

    MoveList list[1];
    generateCaptures(position, list);

    int legal = 0;
    score = -INFINITE;

    for (int i = 0; i < list->count; i++) {

        pickNextMove(i, list);

        if (!makeMove(position, list->moves[i].move)) {
            continue;
        }

        legal++;
        score = -quiescence(position, info, -beta, -alpha);
        unmakeMove(position);

        if (info->stopped != 0) {
            return 0;
        }

        if (score > alpha) {
            if (score >= beta) {
                if (legal == 1) {
                    info->failHighFirst++;
                }
                info->failHigh++;
                return beta;
            }
            alpha = score;
        }
    }

    return alpha;
}

int absoluteValue(int i)
{
    if (i > 0)
    {
        return i;
    }
    else
    {
        return -i;
    }
}

int BigPiecesExist(Board *position, int side)
{
    if (side == 0)
    {
        if(((position->pieceBB[WHITE_KNIGHT]) | (position->pieceBB[WHITE_BISHOP]) | (position->pieceBB[WHITE_ROOK]) | (position->pieceBB[WHITE_QUEEN])) == 0ULL)
        {
            return 0;
        }
        else
        {
            return 1;
        }
    }
    else
    {
        if(((position->pieceBB[BLACK_KNIGHT]) | (position->pieceBB[BLACK_BISHOP]) | (position->pieceBB[BLACK_ROOK]) | (position->pieceBB[BLACK_QUEEN])) == 0ULL)
        {
            return 0;
        }
        else
        {
            return 1;
        }
    }
}

// Null Move Pruning Values
static const int R = 2;
static const int minDepth = 3;

// Razoring Values
static const int RazorDepth = 2;
static const int RazorMargin[3] = { 0, 200, 400 };

// Futility Values
static const int FutilityDepth = 6;
static const int FutilityMargin[7] = { 0, 200, 325, 450, 575, 700, 825 };

// Reverse Futility Values
static const int RevFutilityDepth = 5;
static const int RevFutilityMargin[6] = { 0, 200, 400, 600, 800, 1000 };

// LMR Values
static const int LateMoveDepth = 3;
static const int FullSearchMoves = 2;

static int alphaBeta(Board* position, SearchInfo* info, int alpha, int beta, int depth, int doNull, int DoLMR) {
    // pre-search checks

    const int InCheck = underCheck(position, position->side);

    // Check Extension (Extend all checks before dropping into Quiescence)
    if (InCheck) {
        depth++;
    }
    
    if (depth <= 0) {
        //return evaluatePosition(position);
        return quiescence(position, info, alpha, beta);
    }

    if ((info->nodes & 2047) == 0) {
        checkUp(info);
    }

    info->nodes++;

    if ((isRepetition(position) == 1) || position->fiftyMoveCounter >= 100) {
        return 0;
    }
    if (position->ply >= MAX_DEPTH) {
        return evaluatePosition(position);
    }

    // Mate Distance Pruning (finds shorter mates)
    alpha = MAX(alpha, -INFINITE + position->ply);
    beta = MIN(beta, INFINITE - position->ply);
    if (alpha >= beta) {
        return alpha;
    }

    int pvMove = NOMOVE;
    int score = -INFINITE;
    if (probeHashEntry(position, &pvMove, &score, alpha, beta, depth) != 0) {
        position->HashTable->cut++;
        return score;
    }

    const int positionEval = evaluatePosition(position);

    // Razoring (alpha)
    if (depth <= RazorDepth && !pvMove && !InCheck && positionEval + RazorMargin[depth] <= alpha) {
        // drop into qSearch if move most likely won't beat alpha
        score = quiescence(position, info, alpha - RazorMargin[depth], beta - RazorMargin[depth]);
        if (score + RazorMargin[depth] <= alpha) {
            return score;
        }
    }

    // Reverse Futility Pruning (prunes near beta)
    if (depth <= RevFutilityDepth && !pvMove && !InCheck && abs(beta) < ISMATE && positionEval - RevFutilityMargin[depth] >= beta) {
        return positionEval - RevFutilityMargin[depth];
    }

    // Null Move Pruning
    if (depth >= minDepth && doNull && !InCheck && position->ply && (BigPiecesExist(position, position->side)/*position.bigPce[position.side] > 0*/) && positionEval >= beta) {
        MakeNullMove(position);
        score = -alphaBeta(position, info, -beta, -beta + 1, depth - 1 - R, 0, 0);
        TakeNullMove(position);
        if (info->stopped == 1) {
            return 0;
        }

        if (score >= beta && abs(score) < ISMATE) {
            info->nullCut++;
            return beta;
        }
    }

    MoveList list[1];
    generateMoves(position, list);

    int i = 0;
    int legal = 0;
    int oldAlpha = alpha;
    int bestMove = NOMOVE;

    int bestScore = -INFINITE;

    score = -INFINITE;

    if (pvMove != NOMOVE) {
        for (int i = 0; i < list->count; i++) {
            if (list->moves[i].move == pvMove) {
                list->moves[i].score = 20000000;
                break;
            }
        }
    }

    int FoundPv = 0;

    // Futility Pruning flag (if node is futile (unlikely to raise alpha), this flag is set)
    int FutileNode = (depth <= FutilityDepth && positionEval + FutilityMargin[depth] <= alpha && abs(score) < ISMATE) ? 1 : 0;

    for (int MoveNum = 0; MoveNum < list->count; ++MoveNum) {

        pickNextMove(MoveNum, list);

        // Futility Pruning (if node is considered futile, and at least 1 legal move has been searched, don't search any more quiet moves in the position)
        if (legal && FutileNode && !(list->moves[MoveNum].move & 0xF000) && !(list->moves[MoveNum].move & 0xF00000) && !underCheck(position, position->side)) {
            continue;
        }

        // if move is legal, play it
        if (!makeMove(position, list->moves[MoveNum].move)) {
            continue;
        }

        legal++;

        // PVS (speeds up search with good move ordering)
        if (FoundPv == TRUE) {

            // Late Move Reductions at Root (reduces moves if past full move search limit (not reducing captures, checks, or promotions))
            if (depth >= LateMoveDepth && !(list->moves[MoveNum].move & 0xF000) && !(list->moves[MoveNum].move & 0xF00000) && !underCheck(position, position->side) && DoLMR && legal > FullSearchMoves) {

                // get initial reduction depth
                int reduce = LMRTable[MIN(depth, 63)][MIN(legal, 63)];

                // reduce less for killer moves
                if ((list->moves[MoveNum].score == 800000 || list->moves[MoveNum].score == 900000)) reduce--;

                // do not fall directly into quiescence search
                reduce = MIN(depth - 1, MAX(reduce, 1));

                // print reduction depth at move number
                // printf("reduction: %d depth: %d moveNum: %d\n", (reduce - 1), depth, Legal);

                // search with the reduced depth
                score = -alphaBeta(position, info, -alpha - 1, -alpha, depth - reduce, 1, 0);

            }
            else {
                // If LMR conditions not met (not at root, or tactical move), do a null window search (because we are using PVS)
                score = -alphaBeta(position, info, -alpha - 1, -alpha, depth - 1, 1, 1);

            }
            if (score > alpha&& score < beta) {
                // If the LMR or the null window fails, do a full search
                score = -alphaBeta(position, info, -beta, -alpha, depth - 1, 1, 0);

            }
        }
        else {
            // If no PV found, do a full search
            score = -alphaBeta(position, info, -beta, -alpha, depth - 1, 1, 0);

        }

        //TakeMove(position);
        unmakeMove(position);

        if (info->stopped == 1) {
            return 0;
        }
        if (score > bestScore) {
            bestScore = score;
            bestMove = list->moves[MoveNum].move;
            if (score > alpha) {
                if (score >= beta) {
                    if (legal == 1) {
                        info->failHighFirst++;
                    }
                    info->failHigh++;

                    if (!isCapture(list->moves[i].move)) {
                        position->searchKillers[1][position->ply] = position->searchKillers[0][position->ply];
                        position->searchKillers[0][position->ply] = list->moves[MoveNum].move;
                    }

                    storeHashEntry(position, bestMove, beta, depth, FLAG_BETA);

                    return beta;
                }
                FoundPv = 1;
                alpha = score;

                if (!isCapture(list->moves[i].move))
                {
                    position->searchHistory[moving(bestMove)][to(bestMove)] += depth;
                }
            }
        }
    }

    if (legal == 0) {
        if (underCheck(position, position->side)) {
            return -INFINITE + position->ply;
        }
        else {
            return 0;
        }
    }

    if (oldAlpha != alpha) {
        storeHashEntry(position, bestMove, bestScore, depth, FLAG_EXACT);
    }
    else {
        storeHashEntry(position, bestMove, alpha, depth, FLAG_ALPHA);
    }

    return alpha;
}

void searchPosition(Board* position, SearchInfo* info)
{
    int bestMove = NOMOVE;
    int bestScore = -INFINITE;
    int pvMoves;

    int time;

    clearForSearch(position, info);

    for (int currentDepth = 1; currentDepth <= info->depth; ++currentDepth)
    {
        bestScore = alphaBeta(position, info, -INFINITE, INFINITE, currentDepth, 1, 1);

        if (info->stopped == 1) {
            break;
        }

        pvMoves = getPV(position, currentDepth);
        bestMove = position->pvArray[0];

        if (absoluteValue(bestScore) > ISMATE) {
            bestScore = (bestScore > 0 ? INFINITE - bestScore + 1 : -INFINITE - bestScore) / 2;
            printf("info score mate %d depth %d nodes %ld time %d ",
                bestScore, currentDepth, info->nodes, GetTickCount() - info->startTime);
        }
        else {
            printf("info score cp %d depth %d nodes %ld time %d ",
                bestScore, currentDepth, info->nodes, GetTickCount() - info->startTime);
        }
        
        pvMoves = getPV(position, currentDepth);
        printf("pv");
        for (int i = 0; i < pvMoves; ++i) {
            printf(" %s", PrMove(position->pvArray[i]));
        }
        printf("\n");
    }

    printf("bestmove %s\n", PrMove(bestMove));
}

//uci

#define INPUTBUFFER 400 * 6

void parseGo(Board* position, SearchInfo* info, char* input) {

    int depth = -1, movesToGo = 30, movetime = -1;
    int time = -1, increment = 0;
    char* ptr = NULL;
    info->timeSet = 0;

    if ((ptr = strstr(input, "binc")) && position->side == BLACK) {
        increment = atoi(ptr + 5);
    }

    if ((ptr = strstr(input, "winc")) && position->side == WHITE) {
        increment = atoi(ptr + 5);
    }

    if ((ptr = strstr(input, "wtime")) && position->side == WHITE) {
        time = atoi(ptr + 6);
    }

    if ((ptr = strstr(input, "btime")) && position->side == BLACK) {
        time = atoi(ptr + 6);
    }

    if ((ptr = strstr(input, "movestogo"))) {
        movesToGo = atoi(ptr + 10);
    }

    if ((ptr = strstr(input, "movetime"))) {
        movetime = atoi(ptr + 9);
    }

    if ((ptr = strstr(input, "depth"))) {
        depth = atoi(ptr + 6);
    }

    if (movetime != -1) {
        time = movetime;
        movesToGo = 1;
    }

    info->startTime = GetTickCount();
    info->depth = depth;

    if (time != -1)
    {
        info->timeSet = 1;
        time /= movesToGo;
        time -= 50;
        info->endTime = info->startTime + time + increment;
    }

    if (depth == -1) {
        info->depth = MAX_DEPTH;
    }

    printf("time:%d start:%d stop:%d depth:%d timeset:%d\n",
        time, info->startTime, info->endTime, info->depth, info->timeSet);
    searchPosition(position, info);
}

void parsePosition(Board* position, char* input) {

    input += 9;
    char* ptr = input;

    if (strncmp(input, "startpos", 8) == 0)
    {
        initBoard(position, STARTING_FEN);
    }
    else
    {
        ptr = strstr(input, "fen");
        if (ptr == NULL)
        {
            initBoard(position, STARTING_FEN);
        }
        else
        {
            ptr += 4;
            initBoard(position, ptr);
        }
    }

    ptr = strstr(input, "moves");
    int move;

    if (ptr != NULL) {
        ptr += 6;
        while (*ptr) {
            move = parseMove(position, ptr);
            if (move < 0) break;
            makeMove(position, move);
            position->ply = 0;
            while (*ptr && *ptr != ' ') {
                ptr++;
            }
            ptr++;
        }
    }
    printBoard(position);
}

void uci(Board *position, SearchInfo *info) {
    
    setbuf(stdin, NULL);
    setbuf(stdout, NULL);

    char input[INPUTBUFFER];
    printf("id name Novice 1.0\n");
    printf("id author Jay Warendorff\n");;
    printf("uciok\n");

    while (1) {
        memset(&input[0], 0, sizeof(input));
        fflush(stdout);
        if (!fgets(input, INPUTBUFFER, stdin))
            continue;

        if (input[0] == '\n')
            continue;

        if (!strncmp(input, "isready", 7)) {
            printf("readyok\n");
            continue;
        }
        else if (!strncmp(input, "position", 8)) {
            parsePosition(position, input);
        }
        else if (!strncmp(input, "ucinewgame", 10)) {
            parsePosition(position, "position startpos\n");
        }
        else if (!strncmp(input, "go", 2)) {
            parseGo(position, info, input);
        }
        else if (!strncmp(input, "quit", 4)) {
            info->quit = 1;
        }
        else if (!strncmp(input, "uci", 3)) {
            printf("id name Novice 1.0\n");
            printf("id author Jay Warendorff\n");;
            printf("uciok\n");
        }
        if (info->quit) {
            break;
        }
    }
}

int main() {

    // debug mode variable
    int debug = 0;

    // if debugging - debug 1
    if (debug)
    {
        initAll();
        Board position[1];
        MoveList list[1];
        initBoard(position, STARTING_FEN);
        PerftTest(6, position);//passed 1/21/2021
        while (1);
    }
    else
    {
        initAll();

        Board position[1];
        SearchInfo info[1];
        info->quit = 0;
        position->HashTable->pTable = NULL;
        initHashTable(position->HashTable, 256);

        setbuf(stdin, NULL);
        setbuf(stdout, NULL);

        char input[3000];
        while (1) {
            memset(&input[0], 0, sizeof(input));

            fflush(stdout);
            if (!fgets(input, 256, stdin))
                continue;
            if (input[0] == '\n')
                continue;
            if (!strncmp(input, "uci", 3)) {
                uci(position, info);
                if (info->quit == 1) break;
                continue;
            }
            else if (!strncmp(input, "quit", 4)) {
                break;
            }
        }

        free(position->HashTable->pTable);
    }

	return 0;
}