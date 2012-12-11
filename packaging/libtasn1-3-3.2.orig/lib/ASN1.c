/* A Bison parser, made by GNU Bison 2.5.  */

/* Bison implementation for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2011 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.5"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse         _asn1_yyparse
#define yylex           _asn1_yylex
#define yyerror         _asn1_yyerror
#define yylval          _asn1_yylval
#define yychar          _asn1_yychar
#define yydebug         _asn1_yydebug
#define yynerrs         _asn1_yynerrs


/* Copy the first part of user declarations.  */

/* Line 268 of yacc.c  */
#line 1 "ASN1.y"

/*
 * Copyright (C) 2001-2012 Free Software Foundation, Inc.
 *
 * This file is part of LIBTASN1.
 *
 * The LIBTASN1 library is free software; you can redistribute it
 * and/or modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA
 */

/*****************************************************/
/* File: x509_ASN.y                                  */
/* Description: input file for 'bison' program.      */
/*   The output file is a parser (in C language) for */
/*   ASN.1 syntax                                    */
/*****************************************************/

#include <int.h>
#include <parser_aux.h>
#include <structure.h>
#include <libtasn1.h>

static FILE *file_asn1;			/* Pointer to file to parse */
static int result_parse = 0;	/* result of the parser
					   algorithm */
static asn1_node p_tree;		/* pointer to the root of the
					   structure created by the
					   parser*/
static unsigned int line_number;	/* line number describing the
					   parser position inside the
					   file */
static char last_error[ASN1_MAX_ERROR_DESCRIPTION_SIZE] = "";
static char last_error_token[ASN1_MAX_ERROR_DESCRIPTION_SIZE+1] = ""; /* used when expected errors occur */
static char last_token[ASN1_MAX_NAME_SIZE+1] = ""; /* last token find in the file
					   to parse before the 'parse
					   error' */
extern char _asn1_identifierMissing[];
static const char *file_name;		/* file to parse */

static void _asn1_yyerror (const char *);
static int _asn1_yylex(void);



/* Line 268 of yacc.c  */
#line 136 "ASN1.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     ASSIG = 258,
     NUM = 259,
     IDENTIFIER = 260,
     OPTIONAL = 261,
     INTEGER = 262,
     SIZE = 263,
     OCTET = 264,
     STRING = 265,
     SEQUENCE = 266,
     BIT = 267,
     UNIVERSAL = 268,
     PRIVATE = 269,
     APPLICATION = 270,
     DEFAULT = 271,
     CHOICE = 272,
     OF = 273,
     OBJECT = 274,
     STR_IDENTIFIER = 275,
     BOOLEAN = 276,
     ASN1_TRUE = 277,
     ASN1_FALSE = 278,
     TOKEN_NULL = 279,
     ANY = 280,
     DEFINED = 281,
     BY = 282,
     SET = 283,
     EXPLICIT = 284,
     IMPLICIT = 285,
     DEFINITIONS = 286,
     TAGS = 287,
     BEGIN = 288,
     END = 289,
     UTCTime = 290,
     GeneralizedTime = 291,
     GeneralString = 292,
     NumericString = 293,
     IA5String = 294,
     TeletexString = 295,
     PrintableString = 296,
     UniversalString = 297,
     BMPString = 298,
     UTF8String = 299,
     VisibleString = 300,
     FROM = 301,
     IMPORTS = 302,
     ENUMERATED = 303
   };
#endif
/* Tokens.  */
#define ASSIG 258
#define NUM 259
#define IDENTIFIER 260
#define OPTIONAL 261
#define INTEGER 262
#define SIZE 263
#define OCTET 264
#define STRING 265
#define SEQUENCE 266
#define BIT 267
#define UNIVERSAL 268
#define PRIVATE 269
#define APPLICATION 270
#define DEFAULT 271
#define CHOICE 272
#define OF 273
#define OBJECT 274
#define STR_IDENTIFIER 275
#define BOOLEAN 276
#define ASN1_TRUE 277
#define ASN1_FALSE 278
#define TOKEN_NULL 279
#define ANY 280
#define DEFINED 281
#define BY 282
#define SET 283
#define EXPLICIT 284
#define IMPLICIT 285
#define DEFINITIONS 286
#define TAGS 287
#define BEGIN 288
#define END 289
#define UTCTime 290
#define GeneralizedTime 291
#define GeneralString 292
#define NumericString 293
#define IA5String 294
#define TeletexString 295
#define PrintableString 296
#define UniversalString 297
#define BMPString 298
#define UTF8String 299
#define VisibleString 300
#define FROM 301
#define IMPORTS 302
#define ENUMERATED 303




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 293 of yacc.c  */
#line 62 "ASN1.y"

  unsigned int constant;
  char str[ASN1_MAX_NAME_SIZE+1];
  asn1_node node;



/* Line 293 of yacc.c  */
#line 276 "ASN1.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 343 of yacc.c  */
#line 288 "ASN1.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  5
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   219

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  60
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  51
/* YYNRULES -- Number of rules.  */
#define YYNRULES  125
/* YYNRULES -- Number of states.  */
#define YYNSTATES  217

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   303

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      51,    52,     2,    49,    53,    50,    59,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    54,     2,    55,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    57,    56,    58,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,    12,    14,    17,    20,    22,    24,    26,
      28,    30,    32,    36,    41,    43,    47,    49,    54,    56,
      59,    61,    63,    65,    69,    74,    76,    79,    82,    85,
      88,    91,    93,    97,    99,   104,   109,   117,   119,   121,
     123,   128,   136,   138,   142,   144,   147,   149,   152,   154,
     157,   159,   162,   164,   167,   169,   172,   174,   177,   179,
     182,   184,   187,   190,   194,   199,   201,   205,   208,   212,
     218,   223,   226,   228,   231,   233,   235,   237,   239,   241,
     243,   245,   247,   249,   251,   253,   255,   257,   259,   261,
     263,   265,   267,   269,   271,   273,   275,   278,   280,   283,
     286,   289,   291,   295,   300,   304,   309,   314,   318,   323,
     328,   330,   335,   339,   343,   351,   358,   363,   365,   367,
     369,   372,   377,   381,   383,   385
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      61,     0,    -1,   109,    31,   110,    32,     3,    33,   108,
      34,    -1,     4,    -1,    49,     4,    -1,    50,     4,    -1,
      62,    -1,    63,    -1,     4,    -1,     5,    -1,    64,    -1,
       5,    -1,    51,    64,    52,    -1,     5,    51,    64,    52,
      -1,    67,    -1,    68,    53,    67,    -1,    65,    -1,     5,
      51,     4,    52,    -1,    69,    -1,    70,    69,    -1,    13,
      -1,    14,    -1,    15,    -1,    54,     4,    55,    -1,    54,
      71,     4,    55,    -1,    72,    -1,    72,    29,    -1,    72,
      30,    -1,    16,    66,    -1,    16,    22,    -1,    16,    23,
      -1,    64,    -1,    75,    56,    64,    -1,     7,    -1,     7,
      57,    68,    58,    -1,    76,    51,    75,    52,    -1,    76,
      51,    65,    59,    59,    65,    52,    -1,    21,    -1,    35,
      -1,    36,    -1,     8,    51,    65,    52,    -1,     8,    51,
      65,    59,    59,    65,    52,    -1,    79,    -1,    51,    79,
      52,    -1,    37,    -1,    37,    80,    -1,    38,    -1,    38,
      80,    -1,    39,    -1,    39,    80,    -1,    40,    -1,    40,
      80,    -1,    41,    -1,    41,    80,    -1,    42,    -1,    42,
      80,    -1,    43,    -1,    43,    80,    -1,    44,    -1,    44,
      80,    -1,    45,    -1,    45,    80,    -1,     9,    10,    -1,
       9,    10,    80,    -1,     5,    51,     4,    52,    -1,    91,
      -1,    92,    53,    91,    -1,    12,    10,    -1,    12,    10,
      80,    -1,    12,    10,    57,    92,    58,    -1,    48,    57,
      92,    58,    -1,    19,    20,    -1,     5,    -1,     5,    80,
      -1,    76,    -1,    94,    -1,    77,    -1,    78,    -1,    90,
      -1,    93,    -1,    81,    -1,    82,    -1,    83,    -1,    84,
      -1,    85,    -1,    86,    -1,    87,    -1,    88,    -1,    89,
      -1,   101,    -1,    95,    -1,   103,    -1,   104,    -1,   102,
      -1,    24,    -1,    96,    -1,    73,    96,    -1,    97,    -1,
      97,    74,    -1,    97,     6,    -1,     5,    98,    -1,    99,
      -1,   100,    53,    99,    -1,    11,    57,   100,    58,    -1,
      11,    18,    96,    -1,    11,    80,    18,    96,    -1,    28,
      57,   100,    58,    -1,    28,    18,    96,    -1,    28,    80,
      18,    96,    -1,    17,    57,   100,    58,    -1,    25,    -1,
      25,    26,    27,     5,    -1,     5,     3,    97,    -1,     1,
       3,    97,    -1,     5,    19,    20,     3,    57,    70,    58,
      -1,     5,     5,     3,    57,    70,    58,    -1,     5,     7,
       3,    64,    -1,   105,    -1,   106,    -1,   107,    -1,   108,
     107,    -1,     5,    57,    70,    58,    -1,     5,    57,    58,
      -1,     5,    -1,    29,    -1,    30,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   134,   134,   147,   148,   151,   155,   156,   159,   160,
     163,   164,   167,   169,   174,   175,   179,   181,   186,   187,
     191,   192,   193,   196,   198,   202,   203,   204,   207,   209,
     210,   214,   215,   219,   220,   222,   223,   230,   233,   234,
     237,   239,   245,   246,   249,   250,   254,   255,   259,   260,
     264,   265,   269,   270,   274,   275,   279,   280,   284,   285,
     289,   290,   294,   295,   299,   304,   305,   309,   310,   311,
     316,   322,   325,   327,   330,   331,   332,   333,   334,   335,
     336,   337,   338,   339,   340,   341,   342,   343,   344,   345,
     346,   347,   348,   349,   350,   353,   354,   359,   360,   363,
     366,   369,   370,   374,   376,   378,   383,   385,   387,   392,
     396,   397,   402,   404,   407,   411,   416,   422,   423,   426,
     427,   431,   434,   436,   460,   461
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "\"::=\"", "NUM", "IDENTIFIER",
  "OPTIONAL", "INTEGER", "SIZE", "OCTET", "STRING", "SEQUENCE", "BIT",
  "UNIVERSAL", "PRIVATE", "APPLICATION", "DEFAULT", "CHOICE", "OF",
  "OBJECT", "STR_IDENTIFIER", "BOOLEAN", "ASN1_TRUE", "ASN1_FALSE",
  "TOKEN_NULL", "ANY", "DEFINED", "BY", "SET", "EXPLICIT", "IMPLICIT",
  "DEFINITIONS", "TAGS", "BEGIN", "END", "UTCTime", "GeneralizedTime",
  "GeneralString", "NumericString", "IA5String", "TeletexString",
  "PrintableString", "UniversalString", "BMPString", "UTF8String",
  "VisibleString", "FROM", "IMPORTS", "ENUMERATED", "'+'", "'-'", "'('",
  "')'", "','", "'['", "']'", "'|'", "'{'", "'}'", "'.'", "$accept",
  "definitions", "pos_num", "neg_num", "pos_neg_num", "num_identifier",
  "pos_neg_identifier", "constant", "constant_list", "obj_constant",
  "obj_constant_list", "class", "tag_type", "tag", "default",
  "pos_neg_list", "integer_def", "boolean_def", "Time", "size_def2",
  "size_def", "generalstring_def", "numericstring_def", "ia5string_def",
  "teletexstring_def", "printablestring_def", "universalstring_def",
  "bmpstring_def", "utf8string_def", "visiblestring_def",
  "octet_string_def", "bit_element", "bit_element_list", "bit_string_def",
  "enumerated_def", "object_def", "type_assig_right",
  "type_assig_right_tag", "type_assig_right_tag_default", "type_assig",
  "type_assig_list", "sequence_def", "set_def", "choise_def", "any_def",
  "type_def", "constant_def", "type_constant", "type_constant_list",
  "definitions_id", "explicit_implicit", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,    43,
      45,    40,    41,    44,    91,    93,   124,   123,   125,    46
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    60,    61,    62,    62,    63,    64,    64,    65,    65,
      66,    66,    67,    67,    68,    68,    69,    69,    70,    70,
      71,    71,    71,    72,    72,    73,    73,    73,    74,    74,
      74,    75,    75,    76,    76,    76,    76,    77,    78,    78,
      79,    79,    80,    80,    81,    81,    82,    82,    83,    83,
      84,    84,    85,    85,    86,    86,    87,    87,    88,    88,
      89,    89,    90,    90,    91,    92,    92,    93,    93,    93,
      94,    95,    96,    96,    96,    96,    96,    96,    96,    96,
      96,    96,    96,    96,    96,    96,    96,    96,    96,    96,
      96,    96,    96,    96,    96,    97,    97,    98,    98,    98,
      99,   100,   100,   101,   101,   101,   102,   102,   102,   103,
     104,   104,   105,   105,   106,   106,   106,   107,   107,   108,
     108,   109,   109,   109,   110,   110
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     8,     1,     2,     2,     1,     1,     1,     1,
       1,     1,     3,     4,     1,     3,     1,     4,     1,     2,
       1,     1,     1,     3,     4,     1,     2,     2,     2,     2,
       2,     1,     3,     1,     4,     4,     7,     1,     1,     1,
       4,     7,     1,     3,     1,     2,     1,     2,     1,     2,
       1,     2,     1,     2,     1,     2,     1,     2,     1,     2,
       1,     2,     2,     3,     4,     1,     3,     2,     3,     5,
       4,     2,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     1,     2,     2,
       2,     1,     3,     4,     3,     4,     4,     3,     4,     4,
       1,     4,     3,     3,     7,     6,     4,     1,     1,     1,
       2,     4,     3,     1,     1,     1
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,   123,     0,     0,     0,     1,     0,     8,     9,   122,
      16,    18,     0,   124,   125,     0,     0,   121,    19,     0,
       0,     0,    17,     0,     0,     0,   117,   118,   119,     0,
       0,     0,     0,     0,     0,     2,   120,    72,    33,     0,
       0,     0,     0,     0,    37,    94,   110,     0,    38,    39,
      44,    46,    48,    50,    52,    54,    56,    58,    60,     0,
       0,    25,     0,    74,    76,    77,    80,    81,    82,    83,
      84,    85,    86,    87,    88,    78,    79,    75,    90,    95,
     113,    89,    93,    91,    92,   112,     0,     0,     0,     0,
       0,    42,    73,     0,    62,     0,     0,     0,    67,     0,
      71,     0,     0,     0,     0,    45,    47,    49,    51,    53,
      55,    57,    59,    61,     0,     0,    20,    21,    22,     0,
      26,    27,    96,     0,     0,     3,     0,     0,     6,     7,
     116,     0,     0,     0,     0,     0,    14,     0,    63,   104,
       0,   101,     0,     0,     0,    68,     0,     0,   107,     0,
       0,     0,    65,     0,    23,     0,     3,     9,    31,     0,
       0,     0,     4,     5,     0,     0,    43,     0,     0,     0,
      34,    97,   100,     0,   103,   105,     0,   109,   111,   106,
     108,     0,     0,    70,    24,     0,    35,     0,   115,     0,
      40,     0,     0,    12,    15,    99,     0,    98,   102,    69,
       0,    66,     0,    32,   114,     0,    13,    11,    29,    30,
      10,    28,    64,     0,     0,    36,    41
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,   128,   129,   130,    10,   211,   136,   137,    11,
      12,   119,    61,    62,   197,   160,    63,    64,    65,    91,
      92,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,   152,   153,    76,    77,    78,    79,    80,   172,   141,
     142,    81,    82,    83,    84,    26,    27,    28,    29,     3,
      15
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -121
static const yytype_int16 yypact[] =
{
      10,   -16,    49,    20,     1,  -121,    28,  -121,     9,  -121,
    -121,  -121,     4,  -121,  -121,    44,    69,  -121,  -121,    91,
      55,   100,  -121,   112,   144,    99,  -121,  -121,  -121,    29,
      84,    84,   148,   150,   136,  -121,  -121,     5,   101,   147,
      21,   149,   104,   145,  -121,  -121,   137,    35,  -121,  -121,
       5,     5,     5,     5,     5,     5,     5,     5,     5,   109,
      32,    37,   143,   118,  -121,  -121,  -121,  -121,  -121,  -121,
    -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,
    -121,  -121,  -121,  -121,  -121,  -121,   113,    48,   169,   122,
     166,  -121,  -121,    23,     5,   143,   170,   158,    24,   170,
    -121,   162,   143,   170,   172,  -121,  -121,  -121,  -121,  -121,
    -121,  -121,  -121,  -121,   187,   139,  -121,  -121,  -121,   189,
    -121,  -121,  -121,    50,   138,  -121,   191,   192,  -121,  -121,
    -121,   140,   141,   146,   151,    48,  -121,   -20,  -121,  -121,
      84,  -121,    26,   143,   187,  -121,    58,   194,  -121,    77,
     143,   152,  -121,    78,  -121,   153,   142,  -121,  -121,   154,
      85,     6,  -121,  -121,   138,   -15,  -121,    48,   155,    23,
    -121,    34,  -121,   170,  -121,  -121,    81,  -121,  -121,  -121,
    -121,   196,   187,  -121,  -121,   156,  -121,    48,  -121,    22,
    -121,   157,   159,  -121,  -121,  -121,    65,  -121,  -121,  -121,
     160,  -121,   141,  -121,  -121,   141,  -121,  -121,  -121,  -121,
    -121,  -121,  -121,   165,   167,  -121,  -121
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -121,  -121,  -121,  -121,  -119,  -120,  -121,    36,  -121,   -12,
     -93,  -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,   114,
     -33,  -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,  -121,
    -121,    27,    62,  -121,  -121,  -121,   -60,   -30,  -121,    45,
      41,  -121,  -121,  -121,  -121,  -121,  -121,   181,  -121,  -121,
    -121
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -9
static const yytype_int16 yytable[] =
{
      18,    85,   122,   159,   158,     7,     8,    97,     7,     8,
       7,     8,   165,    89,   104,     1,   168,   105,   106,   107,
     108,   109,   110,   111,   112,   113,     7,     8,   134,    89,
      24,   161,    89,   169,    25,   139,   115,   190,   170,    95,
     195,     4,   148,    89,   191,   116,   117,   118,   192,     5,
     196,     6,   125,   102,   156,   157,    90,    13,    14,     9,
      16,   138,    17,    35,   188,   145,   120,   121,   203,   125,
     207,   189,    90,    20,   135,    90,    19,   210,    96,   173,
     204,   144,   213,   175,   174,   214,    90,   208,   209,    37,
     180,    38,   103,    39,    21,    40,    41,   126,   127,   126,
     127,    42,    31,    43,    32,    44,    33,    22,    45,    46,
     171,   173,    47,    24,   126,   127,   177,    25,    34,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    57,    58,
     173,   182,    59,    23,   182,   179,   183,   186,    60,   199,
     146,   187,     7,     8,   149,     7,   157,    30,    37,    18,
      38,    86,    39,    87,    40,    41,    88,    94,    93,    98,
      42,    99,    43,   101,    44,   100,   114,    45,    46,   123,
     124,    47,   131,   132,    89,   140,   143,    18,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,   147,
     150,    59,   151,   155,   154,   162,   163,   164,   166,   178,
     200,    -8,   167,   181,   133,   194,   176,   193,   184,   201,
      36,   206,   212,   185,     0,   202,   205,   215,   198,   216
};

#define yypact_value_is_default(yystate) \
  ((yystate) == (-121))

#define yytable_value_is_error(yytable_value) \
  YYID (0)

static const yytype_int16 yycheck[] =
{
      12,    31,    62,   123,   123,     4,     5,    40,     4,     5,
       4,     5,   132,     8,    47,     5,   135,    50,    51,    52,
      53,    54,    55,    56,    57,    58,     4,     5,     5,     8,
       1,   124,     8,    53,     5,    95,     4,    52,    58,    18,
       6,    57,   102,     8,    59,    13,    14,    15,   167,     0,
      16,    31,     4,    18,     4,     5,    51,    29,    30,    58,
      51,    94,    58,    34,    58,    98,    29,    30,   187,     4,
       5,   164,    51,     4,    51,    51,    32,   196,    57,    53,
      58,    57,   202,   143,    58,   205,    51,    22,    23,     5,
     150,     7,    57,     9,     3,    11,    12,    49,    50,    49,
      50,    17,     3,    19,     5,    21,     7,    52,    24,    25,
     140,    53,    28,     1,    49,    50,    58,     5,    19,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    44,    45,
      53,    53,    48,    33,    53,    58,    58,    52,    54,    58,
      99,    56,     4,     5,   103,     4,     5,     3,     5,   161,
       7,     3,     9,     3,    11,    12,    20,    10,    57,    10,
      17,    57,    19,    26,    21,    20,    57,    24,    25,    51,
      57,    28,     3,    51,     8,     5,    18,   189,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    27,
      18,    48,     5,     4,    55,     4,     4,    57,    52,     5,
       4,    59,    51,    51,    90,   169,   144,    52,    55,   182,
      29,    52,    52,    59,    -1,    59,    59,    52,   173,    52
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     5,    61,   109,    57,     0,    31,     4,     5,    58,
      65,    69,    70,    29,    30,   110,    51,    58,    69,    32,
       4,     3,    52,    33,     1,     5,   105,   106,   107,   108,
       3,     3,     5,     7,    19,    34,   107,     5,     7,     9,
      11,    12,    17,    19,    21,    24,    25,    28,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    44,    45,    48,
      54,    72,    73,    76,    77,    78,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    93,    94,    95,    96,
      97,   101,   102,   103,   104,    97,     3,     3,    20,     8,
      51,    79,    80,    57,    10,    18,    57,    80,    10,    57,
      20,    26,    18,    57,    80,    80,    80,    80,    80,    80,
      80,    80,    80,    80,    57,     4,    13,    14,    15,    71,
      29,    30,    96,    51,    57,     4,    49,    50,    62,    63,
      64,     3,    51,    79,     5,    51,    67,    68,    80,    96,
       5,    99,   100,    18,    57,    80,   100,    27,    96,   100,
      18,     5,    91,    92,    55,     4,     4,     5,    64,    65,
      75,    70,     4,     4,    57,    65,    52,    51,    64,    53,
      58,    97,    98,    53,    58,    96,    92,    58,     5,    58,
      96,    51,    53,    58,    55,    59,    52,    56,    58,    70,
      52,    59,    64,    52,    67,     6,    16,    74,    99,    58,
       4,    91,    59,    64,    58,    59,    52,     5,    22,    23,
      64,    66,    52,    65,    65,    52,    52
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* This macro is provided for backward compatibility. */

#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (0, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  YYSIZE_T yysize1;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = 0;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - Assume YYFAIL is not used.  It's too flawed to consider.  See
       <http://lists.gnu.org/archive/html/bison-patches/2009-12/msg00024.html>
       for details.  YYERROR is fine as it does not invoke this
       function.
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                yysize1 = yysize + yytnamerr (0, yytname[yyx]);
                if (! (yysize <= yysize1
                       && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                  return 2;
                yysize = yysize1;
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  yysize1 = yysize + yystrlen (yyformat);
  if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
    return 2;
  yysize = yysize1;

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:

/* Line 1806 of yacc.c  */
#line 137 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_DEFINITIONS|(yyvsp[(3) - (8)].constant));
                    _asn1_set_name((yyval.node),_asn1_get_name((yyvsp[(1) - (8)].node)));
                    _asn1_set_name((yyvsp[(1) - (8)].node),"");
                    _asn1_set_right((yyvsp[(1) - (8)].node),(yyvsp[(7) - (8)].node));
                    _asn1_set_down((yyval.node),(yyvsp[(1) - (8)].node));

		    p_tree=(yyval.node);
		    }
    break;

  case 3:

/* Line 1806 of yacc.c  */
#line 147 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(1) - (1)].str));}
    break;

  case 4:

/* Line 1806 of yacc.c  */
#line 148 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(2) - (2)].str));}
    break;

  case 5:

/* Line 1806 of yacc.c  */
#line 151 "ASN1.y"
    {strcpy((yyval.str),"-");
                       strcat((yyval.str),(yyvsp[(2) - (2)].str));}
    break;

  case 6:

/* Line 1806 of yacc.c  */
#line 155 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(1) - (1)].str));}
    break;

  case 7:

/* Line 1806 of yacc.c  */
#line 156 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(1) - (1)].str));}
    break;

  case 8:

/* Line 1806 of yacc.c  */
#line 159 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(1) - (1)].str));}
    break;

  case 9:

/* Line 1806 of yacc.c  */
#line 160 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(1) - (1)].str));}
    break;

  case 10:

/* Line 1806 of yacc.c  */
#line 163 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(1) - (1)].str));}
    break;

  case 11:

/* Line 1806 of yacc.c  */
#line 164 "ASN1.y"
    {strcpy((yyval.str),(yyvsp[(1) - (1)].str));}
    break;

  case 12:

/* Line 1806 of yacc.c  */
#line 167 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_CONSTANT);
                                       _asn1_set_value((yyval.node),(yyvsp[(2) - (3)].str),strlen((yyvsp[(2) - (3)].str))+1);}
    break;

  case 13:

/* Line 1806 of yacc.c  */
#line 169 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_CONSTANT);
	                               _asn1_set_name((yyval.node),(yyvsp[(1) - (4)].str));
                                       _asn1_set_value((yyval.node),(yyvsp[(3) - (4)].str),strlen((yyvsp[(3) - (4)].str))+1);}
    break;

  case 14:

/* Line 1806 of yacc.c  */
#line 174 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 15:

/* Line 1806 of yacc.c  */
#line 175 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (3)].node);
                                            _asn1_set_right(_asn1_get_last_right((yyvsp[(1) - (3)].node)),(yyvsp[(3) - (3)].node));}
    break;

  case 16:

/* Line 1806 of yacc.c  */
#line 179 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_CONSTANT);
                                   _asn1_set_value((yyval.node),(yyvsp[(1) - (1)].str),strlen((yyvsp[(1) - (1)].str))+1);}
    break;

  case 17:

/* Line 1806 of yacc.c  */
#line 181 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_CONSTANT);
	                            _asn1_set_name((yyval.node),(yyvsp[(1) - (4)].str));
                                    _asn1_set_value((yyval.node),(yyvsp[(3) - (4)].str),strlen((yyvsp[(3) - (4)].str))+1);}
    break;

  case 18:

/* Line 1806 of yacc.c  */
#line 186 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 19:

/* Line 1806 of yacc.c  */
#line 187 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (2)].node);
                                                    _asn1_set_right(_asn1_get_last_right((yyvsp[(1) - (2)].node)),(yyvsp[(2) - (2)].node));}
    break;

  case 20:

/* Line 1806 of yacc.c  */
#line 191 "ASN1.y"
    {(yyval.constant)=CONST_UNIVERSAL;}
    break;

  case 21:

/* Line 1806 of yacc.c  */
#line 192 "ASN1.y"
    {(yyval.constant)=CONST_PRIVATE;}
    break;

  case 22:

/* Line 1806 of yacc.c  */
#line 193 "ASN1.y"
    {(yyval.constant)=CONST_APPLICATION;}
    break;

  case 23:

/* Line 1806 of yacc.c  */
#line 196 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_TAG);
                            _asn1_set_value((yyval.node),(yyvsp[(2) - (3)].str),strlen((yyvsp[(2) - (3)].str))+1);}
    break;

  case 24:

/* Line 1806 of yacc.c  */
#line 198 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_TAG | (yyvsp[(2) - (4)].constant));
                                _asn1_set_value((yyval.node),(yyvsp[(3) - (4)].str),strlen((yyvsp[(3) - (4)].str))+1);}
    break;

  case 25:

/* Line 1806 of yacc.c  */
#line 202 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 26:

/* Line 1806 of yacc.c  */
#line 203 "ASN1.y"
    {(yyval.node)=_asn1_mod_type((yyvsp[(1) - (2)].node),CONST_EXPLICIT);}
    break;

  case 27:

/* Line 1806 of yacc.c  */
#line 204 "ASN1.y"
    {(yyval.node)=_asn1_mod_type((yyvsp[(1) - (2)].node),CONST_IMPLICIT);}
    break;

  case 28:

/* Line 1806 of yacc.c  */
#line 207 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_DEFAULT);
                                       _asn1_set_value((yyval.node),(yyvsp[(2) - (2)].str),strlen((yyvsp[(2) - (2)].str))+1);}
    break;

  case 29:

/* Line 1806 of yacc.c  */
#line 209 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_DEFAULT|CONST_TRUE);}
    break;

  case 30:

/* Line 1806 of yacc.c  */
#line 210 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_DEFAULT|CONST_FALSE);}
    break;

  case 33:

/* Line 1806 of yacc.c  */
#line 219 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_INTEGER);}
    break;

  case 34:

/* Line 1806 of yacc.c  */
#line 220 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_INTEGER|CONST_LIST);
	                                 _asn1_set_down((yyval.node),(yyvsp[(3) - (4)].node));}
    break;

  case 35:

/* Line 1806 of yacc.c  */
#line 222 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_INTEGER);}
    break;

  case 36:

/* Line 1806 of yacc.c  */
#line 224 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_INTEGER|CONST_MIN_MAX);
                                         _asn1_set_down((yyval.node),_asn1_add_static_node(ASN1_ETYPE_SIZE));
                                         _asn1_set_value(_asn1_get_down((yyval.node)),(yyvsp[(6) - (7)].str),strlen((yyvsp[(6) - (7)].str))+1);
                                         _asn1_set_name(_asn1_get_down((yyval.node)),(yyvsp[(3) - (7)].str));}
    break;

  case 37:

/* Line 1806 of yacc.c  */
#line 230 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_BOOLEAN);}
    break;

  case 38:

/* Line 1806 of yacc.c  */
#line 233 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_UTC_TIME);}
    break;

  case 39:

/* Line 1806 of yacc.c  */
#line 234 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_GENERALIZED_TIME);}
    break;

  case 40:

/* Line 1806 of yacc.c  */
#line 237 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_SIZE|CONST_1_PARAM);
	                              _asn1_set_value((yyval.node),(yyvsp[(3) - (4)].str),strlen((yyvsp[(3) - (4)].str))+1);}
    break;

  case 41:

/* Line 1806 of yacc.c  */
#line 240 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_SIZE|CONST_MIN_MAX);
	                              _asn1_set_value((yyval.node),(yyvsp[(3) - (7)].str),strlen((yyvsp[(3) - (7)].str))+1);
                                      _asn1_set_name((yyval.node),(yyvsp[(6) - (7)].str));}
    break;

  case 42:

/* Line 1806 of yacc.c  */
#line 245 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 43:

/* Line 1806 of yacc.c  */
#line 246 "ASN1.y"
    {(yyval.node)=(yyvsp[(2) - (3)].node);}
    break;

  case 44:

/* Line 1806 of yacc.c  */
#line 249 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_GENERALSTRING);}
    break;

  case 45:

/* Line 1806 of yacc.c  */
#line 250 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_GENERALSTRING|CONST_SIZE);
					  _asn1_set_down((yyval.node),(yyvsp[(2) - (2)].node));}
    break;

  case 46:

/* Line 1806 of yacc.c  */
#line 254 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_NUMERIC_STRING|CONST_UNIVERSAL);}
    break;

  case 47:

/* Line 1806 of yacc.c  */
#line 255 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_NUMERIC_STRING|CONST_SIZE);
					  _asn1_set_down((yyval.node),(yyvsp[(2) - (2)].node));}
    break;

  case 48:

/* Line 1806 of yacc.c  */
#line 259 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_IA5_STRING);}
    break;

  case 49:

/* Line 1806 of yacc.c  */
#line 260 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_IA5_STRING|CONST_SIZE);
					  _asn1_set_down((yyval.node),(yyvsp[(2) - (2)].node));}
    break;

  case 50:

/* Line 1806 of yacc.c  */
#line 264 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_TELETEX_STRING);}
    break;

  case 51:

/* Line 1806 of yacc.c  */
#line 265 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_TELETEX_STRING|CONST_SIZE);
					  _asn1_set_down((yyval.node),(yyvsp[(2) - (2)].node));}
    break;

  case 52:

/* Line 1806 of yacc.c  */
#line 269 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_PRINTABLE_STRING);}
    break;

  case 53:

/* Line 1806 of yacc.c  */
#line 270 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_PRINTABLE_STRING|CONST_SIZE);
					  _asn1_set_down((yyval.node),(yyvsp[(2) - (2)].node));}
    break;

  case 54:

/* Line 1806 of yacc.c  */
#line 274 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_UNIVERSAL_STRING);}
    break;

  case 55:

/* Line 1806 of yacc.c  */
#line 275 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_UNIVERSAL_STRING|CONST_SIZE);
					  _asn1_set_down((yyval.node),(yyvsp[(2) - (2)].node));}
    break;

  case 56:

/* Line 1806 of yacc.c  */
#line 279 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_BMP_STRING);}
    break;

  case 57:

/* Line 1806 of yacc.c  */
#line 280 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_BMP_STRING|CONST_SIZE);
					  _asn1_set_down((yyval.node),(yyvsp[(2) - (2)].node));}
    break;

  case 58:

/* Line 1806 of yacc.c  */
#line 284 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_UTF8_STRING);}
    break;

  case 59:

/* Line 1806 of yacc.c  */
#line 285 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_UTF8_STRING|CONST_SIZE);
					  _asn1_set_down((yyval.node),(yyvsp[(2) - (2)].node));}
    break;

  case 60:

/* Line 1806 of yacc.c  */
#line 289 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_VISIBLE_STRING);}
    break;

  case 61:

/* Line 1806 of yacc.c  */
#line 290 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_VISIBLE_STRING|CONST_SIZE);
					  _asn1_set_down((yyval.node),(yyvsp[(2) - (2)].node));}
    break;

  case 62:

/* Line 1806 of yacc.c  */
#line 294 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_OCTET_STRING);}
    break;

  case 63:

/* Line 1806 of yacc.c  */
#line 295 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_OCTET_STRING|CONST_SIZE);
                                           _asn1_set_down((yyval.node),(yyvsp[(3) - (3)].node));}
    break;

  case 64:

/* Line 1806 of yacc.c  */
#line 299 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_CONSTANT);
	                           _asn1_set_name((yyval.node),(yyvsp[(1) - (4)].str));
                                    _asn1_set_value((yyval.node),(yyvsp[(3) - (4)].str),strlen((yyvsp[(3) - (4)].str))+1);}
    break;

  case 65:

/* Line 1806 of yacc.c  */
#line 304 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 66:

/* Line 1806 of yacc.c  */
#line 305 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (3)].node);
                                                       _asn1_set_right(_asn1_get_last_right((yyvsp[(1) - (3)].node)),(yyvsp[(3) - (3)].node));}
    break;

  case 67:

/* Line 1806 of yacc.c  */
#line 309 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_BIT_STRING);}
    break;

  case 68:

/* Line 1806 of yacc.c  */
#line 310 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_BIT_STRING|CONST_SIZE);}
    break;

  case 69:

/* Line 1806 of yacc.c  */
#line 312 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_BIT_STRING|CONST_LIST);
                                _asn1_set_down((yyval.node),(yyvsp[(4) - (5)].node));}
    break;

  case 70:

/* Line 1806 of yacc.c  */
#line 317 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_ENUMERATED|CONST_LIST);
                                _asn1_set_down((yyval.node),(yyvsp[(3) - (4)].node));}
    break;

  case 71:

/* Line 1806 of yacc.c  */
#line 322 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_OBJECT_ID);}
    break;

  case 72:

/* Line 1806 of yacc.c  */
#line 325 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_IDENTIFIER);
                                       _asn1_set_value((yyval.node),(yyvsp[(1) - (1)].str),strlen((yyvsp[(1) - (1)].str))+1);}
    break;

  case 73:

/* Line 1806 of yacc.c  */
#line 327 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_IDENTIFIER|CONST_SIZE);
                                       _asn1_set_value((yyval.node),(yyvsp[(1) - (2)].str),strlen((yyvsp[(1) - (2)].str))+1);
                                       _asn1_set_down((yyval.node),(yyvsp[(2) - (2)].node));}
    break;

  case 74:

/* Line 1806 of yacc.c  */
#line 330 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 75:

/* Line 1806 of yacc.c  */
#line 331 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 76:

/* Line 1806 of yacc.c  */
#line 332 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 78:

/* Line 1806 of yacc.c  */
#line 334 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 79:

/* Line 1806 of yacc.c  */
#line 335 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 80:

/* Line 1806 of yacc.c  */
#line 336 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 81:

/* Line 1806 of yacc.c  */
#line 337 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 82:

/* Line 1806 of yacc.c  */
#line 338 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 83:

/* Line 1806 of yacc.c  */
#line 339 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 84:

/* Line 1806 of yacc.c  */
#line 340 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 85:

/* Line 1806 of yacc.c  */
#line 341 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 86:

/* Line 1806 of yacc.c  */
#line 342 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 87:

/* Line 1806 of yacc.c  */
#line 343 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 88:

/* Line 1806 of yacc.c  */
#line 344 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 89:

/* Line 1806 of yacc.c  */
#line 345 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 90:

/* Line 1806 of yacc.c  */
#line 346 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 91:

/* Line 1806 of yacc.c  */
#line 347 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 92:

/* Line 1806 of yacc.c  */
#line 348 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 93:

/* Line 1806 of yacc.c  */
#line 349 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 94:

/* Line 1806 of yacc.c  */
#line 350 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_NULL);}
    break;

  case 95:

/* Line 1806 of yacc.c  */
#line 353 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 96:

/* Line 1806 of yacc.c  */
#line 354 "ASN1.y"
    {(yyval.node)=_asn1_mod_type((yyvsp[(2) - (2)].node),CONST_TAG);
                                               _asn1_set_right((yyvsp[(1) - (2)].node),_asn1_get_down((yyval.node)));
                                               _asn1_set_down((yyval.node),(yyvsp[(1) - (2)].node));}
    break;

  case 97:

/* Line 1806 of yacc.c  */
#line 359 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 98:

/* Line 1806 of yacc.c  */
#line 360 "ASN1.y"
    {(yyval.node)=_asn1_mod_type((yyvsp[(1) - (2)].node),CONST_DEFAULT);
                                                       _asn1_set_right((yyvsp[(2) - (2)].node),_asn1_get_down((yyval.node)));
						       _asn1_set_down((yyval.node),(yyvsp[(2) - (2)].node));}
    break;

  case 99:

/* Line 1806 of yacc.c  */
#line 363 "ASN1.y"
    {(yyval.node)=_asn1_mod_type((yyvsp[(1) - (2)].node),CONST_OPTION);}
    break;

  case 100:

/* Line 1806 of yacc.c  */
#line 366 "ASN1.y"
    {(yyval.node)=_asn1_set_name((yyvsp[(2) - (2)].node),(yyvsp[(1) - (2)].str));}
    break;

  case 101:

/* Line 1806 of yacc.c  */
#line 369 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 102:

/* Line 1806 of yacc.c  */
#line 370 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (3)].node);
                                                _asn1_set_right(_asn1_get_last_right((yyvsp[(1) - (3)].node)),(yyvsp[(3) - (3)].node));}
    break;

  case 103:

/* Line 1806 of yacc.c  */
#line 374 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_SEQUENCE);
                                              _asn1_set_down((yyval.node),(yyvsp[(3) - (4)].node));}
    break;

  case 104:

/* Line 1806 of yacc.c  */
#line 376 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_SEQUENCE_OF);
                                              _asn1_set_down((yyval.node),(yyvsp[(3) - (3)].node));}
    break;

  case 105:

/* Line 1806 of yacc.c  */
#line 378 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_SEQUENCE_OF|CONST_SIZE);
                                            _asn1_set_right((yyvsp[(2) - (4)].node),(yyvsp[(4) - (4)].node));
                                            _asn1_set_down((yyval.node),(yyvsp[(2) - (4)].node));}
    break;

  case 106:

/* Line 1806 of yacc.c  */
#line 383 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_SET);
                                     _asn1_set_down((yyval.node),(yyvsp[(3) - (4)].node));}
    break;

  case 107:

/* Line 1806 of yacc.c  */
#line 385 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_SET_OF);
                                     _asn1_set_down((yyval.node),(yyvsp[(3) - (3)].node));}
    break;

  case 108:

/* Line 1806 of yacc.c  */
#line 387 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_SET_OF|CONST_SIZE);
                                       _asn1_set_right((yyvsp[(2) - (4)].node),(yyvsp[(4) - (4)].node));
                                       _asn1_set_down((yyval.node),(yyvsp[(2) - (4)].node));}
    break;

  case 109:

/* Line 1806 of yacc.c  */
#line 392 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_CHOICE);
                                             _asn1_set_down((yyval.node),(yyvsp[(3) - (4)].node));}
    break;

  case 110:

/* Line 1806 of yacc.c  */
#line 396 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_ANY);}
    break;

  case 111:

/* Line 1806 of yacc.c  */
#line 397 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_ANY|CONST_DEFINED_BY);
                                        _asn1_set_down((yyval.node),_asn1_add_static_node(ASN1_ETYPE_CONSTANT));
	                                _asn1_set_name(_asn1_get_down((yyval.node)),(yyvsp[(4) - (4)].str));}
    break;

  case 112:

/* Line 1806 of yacc.c  */
#line 402 "ASN1.y"
    {(yyval.node)=_asn1_set_name((yyvsp[(3) - (3)].node),(yyvsp[(1) - (3)].str));}
    break;

  case 113:

/* Line 1806 of yacc.c  */
#line 404 "ASN1.y"
    {(yyval.node)=_asn1_set_name((yyvsp[(3) - (3)].node), last_error_token);}
    break;

  case 114:

/* Line 1806 of yacc.c  */
#line 408 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_OBJECT_ID|CONST_ASSIGN);
                         _asn1_set_name((yyval.node),(yyvsp[(1) - (7)].str));
                         _asn1_set_down((yyval.node),(yyvsp[(6) - (7)].node));}
    break;

  case 115:

/* Line 1806 of yacc.c  */
#line 412 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_OBJECT_ID|CONST_ASSIGN|CONST_1_PARAM);
                         _asn1_set_name((yyval.node),(yyvsp[(1) - (6)].str));
                         _asn1_set_value((yyval.node),(yyvsp[(2) - (6)].str),strlen((yyvsp[(2) - (6)].str))+1);
                         _asn1_set_down((yyval.node),(yyvsp[(5) - (6)].node));}
    break;

  case 116:

/* Line 1806 of yacc.c  */
#line 417 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_INTEGER|CONST_ASSIGN);
                         _asn1_set_name((yyval.node),(yyvsp[(1) - (4)].str));
                         _asn1_set_value((yyval.node),(yyvsp[(4) - (4)].str),strlen((yyvsp[(4) - (4)].str))+1);}
    break;

  case 117:

/* Line 1806 of yacc.c  */
#line 422 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 118:

/* Line 1806 of yacc.c  */
#line 423 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 119:

/* Line 1806 of yacc.c  */
#line 426 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (1)].node);}
    break;

  case 120:

/* Line 1806 of yacc.c  */
#line 427 "ASN1.y"
    {(yyval.node)=(yyvsp[(1) - (2)].node);
                                                          _asn1_set_right(_asn1_get_last_right((yyvsp[(1) - (2)].node)),(yyvsp[(2) - (2)].node));}
    break;

  case 121:

/* Line 1806 of yacc.c  */
#line 431 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_OBJECT_ID);
                                                          _asn1_set_down((yyval.node),(yyvsp[(3) - (4)].node));
                                                          _asn1_set_name((yyval.node),(yyvsp[(1) - (4)].str));}
    break;

  case 122:

/* Line 1806 of yacc.c  */
#line 434 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_OBJECT_ID);
                                                          _asn1_set_name((yyval.node),(yyvsp[(1) - (3)].str));}
    break;

  case 123:

/* Line 1806 of yacc.c  */
#line 436 "ASN1.y"
    {(yyval.node)=_asn1_add_static_node(ASN1_ETYPE_OBJECT_ID);
                                                          _asn1_set_name((yyval.node),(yyvsp[(1) - (1)].str));}
    break;

  case 124:

/* Line 1806 of yacc.c  */
#line 460 "ASN1.y"
    {(yyval.constant)=CONST_EXPLICIT;}
    break;

  case 125:

/* Line 1806 of yacc.c  */
#line 461 "ASN1.y"
    {(yyval.constant)=CONST_IMPLICIT;}
    break;



/* Line 1806 of yacc.c  */
#line 2648 "ASN1.c"
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 2067 of yacc.c  */
#line 465 "ASN1.y"




static const char *key_word[] = {
  "::=","OPTIONAL","INTEGER","SIZE","OCTET","STRING",
  "SEQUENCE","BIT","UNIVERSAL","PRIVATE","OPTIONAL",
  "DEFAULT","CHOICE","OF","OBJECT","IDENTIFIER",
  "BOOLEAN","TRUE","FALSE","APPLICATION","ANY","DEFINED",
  "SET","BY","EXPLICIT","IMPLICIT","DEFINITIONS","TAGS",
  "BEGIN","END","UTCTime","GeneralizedTime",
  "GeneralString","FROM","IMPORTS","NULL","ENUMERATED",
  "NumericString", "IA5String", "TeletexString", "PrintableString",
  "UniversalString", "BMPString", "UTF8String", "VisibleString"};

static const int key_word_token[] = {
  ASSIG, OPTIONAL, INTEGER, SIZE, OCTET, STRING, SEQUENCE, BIT, UNIVERSAL,
      PRIVATE, OPTIONAL, DEFAULT, CHOICE, OF, OBJECT, STR_IDENTIFIER,
      BOOLEAN, ASN1_TRUE, ASN1_FALSE, APPLICATION, ANY, DEFINED, SET, BY,
      EXPLICIT, IMPLICIT, DEFINITIONS, TAGS, BEGIN, END, UTCTime,
      GeneralizedTime, GeneralString, FROM, IMPORTS, TOKEN_NULL,
      ENUMERATED, NumericString, IA5String, TeletexString, PrintableString,
      UniversalString, BMPString, UTF8String, VisibleString
};

/*************************************************************/
/*  Function: _asn1_yylex                                    */
/*  Description: looks for tokens in file_asn1 pointer file. */
/*  Return: int                                              */
/*    Token identifier or ASCII code or 0(zero: End Of File) */
/*************************************************************/
static int
_asn1_yylex ()
{
  int c, counter = 0, k, lastc;
  char string[ASN1_MAX_NAME_SIZE + 1];  /* will contain the next token */
  size_t i;

  while (1)
    {
      while ((c = fgetc (file_asn1)) == ' ' || c == '\t' || c == '\n')
        if (c == '\n')
          line_number++;

      if (c == EOF)
        {
          strcpy (last_token, "End Of File");
          return 0;
        }

      if (c == '(' || c == ')' || c == '[' || c == ']' ||
          c == '{' || c == '}' || c == ',' || c == '.' ||
          c == '+' || c == '|')
        {
          last_token[0] = c;
          last_token[1] = 0;
          return c;
        }
      if (c == '-')
        {                       /* Maybe the first '-' of a comment */
          if ((c = fgetc (file_asn1)) != '-')
            {
              ungetc (c, file_asn1);
              last_token[0] = '-';
              last_token[1] = 0;
              return '-';
            }
          else
            {                   /* Comments */
              lastc = 0;
              counter = 0;
              /* A comment finishes at the next double hypen or the end of line */
              while ((c = fgetc (file_asn1)) != EOF && c != '\n' &&
                     (lastc != '-' || (lastc == '-' && c != '-')))
                lastc = c;
              if (c == EOF)
                {
                  strcpy (last_token, "End Of File");
                  return 0;
                }
              else
                {
                  if (c == '\n')
                    line_number++;
                  continue;     /* next char, please! (repeat the search) */
                }
            }
        }
      string[counter++] = c;
      /* Till the end of the token */
      while (!
             ((c = fgetc (file_asn1)) == EOF || c == ' ' || c == '\t'
              || c == '\n' || c == '(' || c == ')' || c == '[' || c == ']'
              || c == '{' || c == '}' || c == ',' || c == '.'))
        {
          if (counter >= ASN1_MAX_NAME_SIZE)
            {
              result_parse = ASN1_NAME_TOO_LONG;
              return 0;
            }
          string[counter++] = c;
        }
      ungetc (c, file_asn1);
      string[counter] = 0;
      strcpy (last_token, string);

      /* Is STRING a number? */
      for (k = 0; k < counter; k++)
        if (!isdigit (string[k]))
          break;
      if (k >= counter)
        {
          strcpy (yylval.str, string);
          return NUM;           /* return the number */
        }

      /* Is STRING a keyword? */
      for (i = 0; i < (sizeof (key_word) / sizeof (char *)); i++)
        if (!strcmp (string, key_word[i]))
          return key_word_token[i];

      /* STRING is an IDENTIFIER */
      strcpy (yylval.str, string);
      return IDENTIFIER;
    }
}

/*************************************************************/
/*  Function: _asn1_create_errorDescription                  */
/*  Description: creates a string with the description of the*/
/*    error.                                                 */
/*  Parameters:                                              */
/*    error : error to describe.                             */
/*    error_desc: string that will contain the         */
/*                      description.                         */
/*************************************************************/
static void
_asn1_create_errorDescription (int error, char *error_desc)
{
  if (error_desc == NULL)
    return;


  switch (error)
    {
    case ASN1_FILE_NOT_FOUND:
      snprintf(error_desc, ASN1_MAX_ERROR_DESCRIPTION_SIZE, "%s file was not found", file_name);
      break;
    case ASN1_SYNTAX_ERROR:
      strcpy(error_desc, last_error);
      break;
    case ASN1_NAME_TOO_LONG:
      snprintf (error_desc, ASN1_MAX_ERROR_DESCRIPTION_SIZE,
                "%s:%u: name too long (more than %u characters)", file_name,
                line_number, ASN1_MAX_NAME_SIZE);
      break;
    case ASN1_IDENTIFIER_NOT_FOUND:
      snprintf (error_desc, ASN1_MAX_ERROR_DESCRIPTION_SIZE,
                "%s:: identifier '%s' not found", file_name,
                _asn1_identifierMissing);
      break;
    default:
      error_desc[0] = 0;
      break;
    }

}

/**
 * asn1_parser2tree:
 * @file: specify the path and the name of file that contains
 *   ASN.1 declarations.
 * @definitions: return the pointer to the structure created from
 *   "file" ASN.1 declarations.
 * @error_desc: return the error description or an empty
 * string if success.
 *
 * Function used to start the parse algorithm.  Creates the structures
 * needed to manage the definitions included in @file file.
 *
 * Returns: %ASN1_SUCCESS if the file has a correct syntax and every
 *   identifier is known, %ASN1_ELEMENT_NOT_EMPTY if @definitions not
 *   %NULL, %ASN1_FILE_NOT_FOUND if an error occured while
 *   opening @file, %ASN1_SYNTAX_ERROR if the syntax is not
 *   correct, %ASN1_IDENTIFIER_NOT_FOUND if in the file there is an
 *   identifier that is not defined, %ASN1_NAME_TOO_LONG if in the
 *   file there is an identifier whith more than %ASN1_MAX_NAME_SIZE
 *   characters.
 **/
int
asn1_parser2tree (const char *file, asn1_node * definitions,
                  char *error_desc)
{

  p_tree = NULL;

  if (*definitions != NULL)
    return ASN1_ELEMENT_NOT_EMPTY;

  *definitions = NULL;

  file_name = file;

  /* open the file to parse */
  file_asn1 = fopen (file, "r");

  if (file_asn1 == NULL)
    {
      result_parse = ASN1_FILE_NOT_FOUND;
    }
  else
    {
      result_parse = ASN1_SUCCESS;

      line_number = 1;
      yyparse ();

      fclose (file_asn1);

      if (result_parse == ASN1_SUCCESS)
        {                       /* syntax OK */
          /* set IMPLICIT or EXPLICIT property */
          _asn1_set_default_tag (p_tree);
          /* set CONST_SET and CONST_NOT_USED */
          _asn1_type_set_config (p_tree);
          /* check the identifier definitions */
          result_parse = _asn1_check_identifier (p_tree);
          if (result_parse == ASN1_SUCCESS)
            {                   /* all identifier defined */
              /* Delete the list and keep the ASN1 structure */
              _asn1_delete_list ();
              /* Convert into DER coding the value assign to INTEGER constants */
              _asn1_change_integer_value (p_tree);
              /* Expand the IDs of OBJECT IDENTIFIER constants */
              _asn1_expand_object_id (p_tree);

              *definitions = p_tree;
            }
          else                  /* some identifiers not defined */
            /* Delete the list and the ASN1 structure */
            _asn1_delete_list_and_nodes ();
        }
      else                      /* syntax error */
        /* Delete the list and the ASN1 structure */
        _asn1_delete_list_and_nodes ();
    }

  _asn1_create_errorDescription (result_parse, error_desc);

  return result_parse;
}

/**
 * asn1_parser2array:
 * @inputFileName: specify the path and the name of file that
 *   contains ASN.1 declarations.
 * @outputFileName: specify the path and the name of file that will
 *   contain the C vector definition.
 * @vectorName: specify the name of the C vector.
 * @error_desc : return the error description or an empty
 *   string if success.
 *
 * Function that generates a C structure from an ASN1 file.  Creates a
 * file containing a C vector to use to manage the definitions
 * included in @inputFileName file. If @inputFileName is
 * "/aa/bb/xx.yy" and @outputFileName is %NULL, the file created is
 * "/aa/bb/xx_asn1_tab.c".  If @vectorName is %NULL the vector name
 * will be "xx_asn1_tab".
 *
 * Returns: %ASN1_SUCCESS if the file has a correct syntax and every
 *   identifier is known, %ASN1_FILE_NOT_FOUND if an error occured
 *   while opening @inputFileName, %ASN1_SYNTAX_ERROR if the syntax is
 *   not correct, %ASN1_IDENTIFIER_NOT_FOUND if in the file there is
 *   an identifier that is not defined, %ASN1_NAME_TOO_LONG if in the
 *   file there is an identifier whith more than %ASN1_MAX_NAME_SIZE
 *   characters.
 **/
int
asn1_parser2array (const char *inputFileName, const char *outputFileName,
                   const char *vectorName, char *error_desc)
{
  char *file_out_name = NULL;
  char *vector_name = NULL;
  const char *char_p, *slash_p, *dot_p;

  p_tree = NULL;

  file_name = inputFileName;

  /* open the file to parse */
  file_asn1 = fopen (inputFileName, "r");

  if (file_asn1 == NULL)
    result_parse = ASN1_FILE_NOT_FOUND;
  else
    {
      result_parse = ASN1_SUCCESS;

      line_number = 1;
      yyparse ();

      fclose (file_asn1);

      if (result_parse == ASN1_SUCCESS)
        {                       /* syntax OK */
          /* set IMPLICIT or EXPLICIT property */
          _asn1_set_default_tag (p_tree);
          /* set CONST_SET and CONST_NOT_USED */
          _asn1_type_set_config (p_tree);
          /* check the identifier definitions */
          result_parse = _asn1_check_identifier (p_tree);

          if (result_parse == ASN1_SUCCESS)
            {                   /* all identifier defined */

              /* searching the last '/' and '.' in inputFileName */
              char_p = inputFileName;
              slash_p = inputFileName;
              while ((char_p = strchr (char_p, '/')))
                {
                  char_p++;
                  slash_p = char_p;
                }

              char_p = slash_p;
              dot_p = inputFileName + strlen (inputFileName);

              while ((char_p = strchr (char_p, '.')))
                {
                  dot_p = char_p;
                  char_p++;
                }

              if (outputFileName == NULL)
                {
                  /* file_out_name = inputFileName + _asn1_tab.c */
                  file_out_name = malloc (dot_p - inputFileName + 1 +
                                          strlen ("_asn1_tab.c"));
                  memcpy (file_out_name, inputFileName,
                          dot_p - inputFileName);
                  file_out_name[dot_p - inputFileName] = 0;
                  strcat (file_out_name, "_asn1_tab.c");
                }
              else
                {
                  /* file_out_name = inputFileName */
                  file_out_name =
                      (char *) malloc (strlen (outputFileName) + 1);
                  strcpy (file_out_name, outputFileName);
                }

              if (vectorName == NULL)
                {
                  /* vector_name = file name + _asn1_tab */
                  vector_name = malloc (dot_p - slash_p + 1 +
                                        strlen ("_asn1_tab"));
                  memcpy (vector_name, slash_p, dot_p - slash_p);
                  vector_name[dot_p - slash_p] = 0;
                  strcat (vector_name, "_asn1_tab");
                }
              else
                {
                  /* vector_name = vectorName */
                  vector_name = (char *) malloc (strlen (vectorName) + 1);
                  strcpy (vector_name, vectorName);
                }

              /* Save structure in a file */
              _asn1_create_static_structure (p_tree,
                                             file_out_name, vector_name);

              free (file_out_name);
              free (vector_name);
            }                   /* result == OK */
        }                       /* result == OK */

      /* Delete the list and the ASN1 structure */
      _asn1_delete_list_and_nodes ();
    }                           /* inputFile exist */

  _asn1_create_errorDescription (result_parse, error_desc);

  return result_parse;
}

/*************************************************************/
/*  Function: _asn1_yyerror                                  */
/*  Description: function called when there are syntax errors*/
/*  Parameters:                                              */
/*    char *s : error description                            */
/*  Return: int                                              */
/*                                                           */
/*************************************************************/
static void
_asn1_yyerror (const char *s)
{
  /* Sends the error description to the std_out */

  if (strcmp (last_token, "VisibleString") == 0 ||
      strcmp (last_token, "PrintableString") == 0 ||
      strcmp (last_token, "UniversalString") == 0 ||
      strcmp (last_token, "IA5String") == 0 ||
      strcmp (last_token, "UTF8String") == 0 ||
      strcmp (last_token, "NumericString") == 0 ||
      strcmp (last_token, "TeletexString") == 0 ||
      strcmp (last_token, "BMPString") == 0)
    {
      snprintf (last_error_token, sizeof(last_error_token),
                "%s", last_token);
      fprintf(stderr, 
               "%s:%u: Warning: %s is a built-in ASN.1 type.\n",
               file_name, line_number, last_token);
      return;
    }
  last_error_token[0] = 0;

  if (result_parse != ASN1_NAME_TOO_LONG)
    {
      snprintf (last_error, sizeof(last_error),
                "%s:%u: Error: %s near '%s'", file_name,
                line_number, s, last_token);
      result_parse = ASN1_SYNTAX_ERROR;
    }

  return;
}

