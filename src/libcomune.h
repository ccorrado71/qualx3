#ifndef LIBCOMUNE_H
#define LIBCOMUNE_H
#include <string>
#include <algorithm>
#include <QColor>
#include <QFile>
#include <QTextStream>
#include <QtCore/QDebug>
#include <QtCore/QDataStream>
#include <QtCore/QSettings>
#include "colorwidget.h"

static const int N_ELEMENTS = 500; //(int) (sizeof(JAV_ELEMENTY)/sizeof(JavElement));
#define SCALA        1.2f
#define MAX_SPECIES  40
#define MAX_LABLEN   25
#define MAX_NUM_MODELS   5
#define MAX_NUM_MAPS     5
#define NATS_LIM         1000
#define nzc              100

typedef enum {NO_MOVE = -1, ROT, ZOOM, TRASL, SELECT_CENTRE, SELECT_DEST,
              GEOM = 10, ON_SELECT = 20, MOL_SELECT, MOVE_LEGEND,
              RECT_SELECT, ZOOM_MODE}   MOTION_TYPE;
//                   =  -1      --> Normal mode / no motion
//                   =  0       --> Rotation
//                   =  1       --> Translation
//                   =  2       --> mouse drag
//                   =  3       --> Select Centre of plot
//                   =  4       --> Select Residue  (right click)
//                   =  11      --> Select 1 atoms  (sphere centre)
//                   =  12      --> Select 2 atoms  (distance)
//                   =  13      --> Select 3 atoms  (angle)
//                   =  14      --> Select 4 atoms  (torsion angle)
//                   =  20      --> Atom Selection mode on
//                   =  21      --> Molecule Selection mode on
//                   =  22      --> Move Legend
//                   =  23      --> Rectangular selection


// Elements etc.
typedef struct {
  char szSymbol[8];
  char szString[20];
  QColor color;
  int   Z;
} JavElement;


typedef struct {
  char szSymbol[5];
  char szCode[2];
  char szString[30];
  QColor color;
} JavAmino;

typedef struct {
  char szString[6];
  QColor color;
} JavChain;


typedef struct {
  char szString[8];
  QColor color;
} JavStructure;

static JavElement JAV_ELEMENTY[N_ELEMENTS] = {
    {"H",  "Hydrogen",         "#d9d9ff",   1},
    {"He", "Helium",           "#d9ffff",   2},
    {"Li", "Lithium",          "#cc80ff",   3},
    {"Be", "Beryllium",        "#c2ff00",   4},
    {"B",  "Boron",            "#ffb5b5",   5},
    {"C",  "Carbon",           "#909090",   6},
    {"N",  "Nitrogen",         "#3050f8",   7},
    {"O",  "Oxygen",           "#ff0d0d",   8},
    {"F",  "Fluorine",         "#90e050",   9},
    {"Ne", "Neon",             "#b3e3f5",  10},
    {"Na", "Sodium",           "#ab5cf2",  11},
    {"Mg", "Magnesium",        "#8aff00",  12},
    {"Al", "Aluminium",        "#bfa6a6",  13},
    {"Si", "Silicon",          "#f0c8a0",  14},
    {"P",  "Phosphorus",       "#ff8000",  15},
    {"S",  "Sulphur",          "#ffff30",  16},
    {"Cl", "Chlorine",         "#1ff01f",  17},
    {"Ar", "Argon",            "#80d1e3",  18},
    {"K",  "Potassium",        "#8f40d4",  19},
    {"Ca", "Calcium",          "#3dff00",  20},
    {"Sc", "Scandium",         "#e6e6e6",  21},
    {"Ti", "Titanium",         "#bfc2c7",  22},
    {"V",  "Vanadium",         "#a6a6ab",  23},
    {"Cr", "Chromium",         "#8a99c7",  24},
    {"Mn", "Manganese",        "#9c7ac7",  25},
    {"Fe", "Iron",             "#e06633",  26},
    {"Co", "Cobalt",           "#f090a0",  27},
    {"Ni", "Nickel",           "#50d050",  28},
    {"Cu", "Copper",           "#c88033",  29},
    {"Zn", "Zinc",             "#7d80b0",  30},
    {"Ga", "Gallium",          "#c28f8f",  31},
    {"Ge", "Germanium",        "#668f8f",  32},
    {"As", "Arsenic",          "#bd80e3",  33},
    {"Se", "Selenium",         "#ffa100",  34},
    {"Br", "Bromine",          "#a62929",  35},
    {"Kr", "Krypton",          "#5cb8d1",  36},
    {"Rb", "Rubidium",         "#702eb0",  37},
    {"Sr", "Strontium",        "#00ff00",  38},
    {"Y",  "Ytrium ",          "#94ffff",  39},
    {"Zr", "Zirconium",        "#94e0e0",  40},
    {"Nb", "Niobium",          "#73c2c9",  41},
    {"Mo", "Molybdenum",       "#54b5b5",  42},
    {"Tc", "Technetium",       "#3b9e9e",  43},
    {"Ru", "Ruthenium",        "#248f8f",  44},
    {"Rh", "Rhodium",          "#0a7d8c",  45},
    {"Pd", "Palladium",        "#006985",  46},
    {"Ag", "Silver",           "#c0c0c0",  47},
    {"Cd", "Cadmium",          "#ffd98f",  48},
    {"In", "Indium",           "#a67573",  49},
    {"Sn", "Tin",              "#668080",  50},
    {"Sb", "Antimony",         "#9e63b5",  51},
    {"Te", "Tellurium",        "#d47a00",  52},
    {"I",  "Iodine",           "#940094",  53},
    {"Xe", "Xenon",            "#429eb0",  54},
    {"Cs", "Caesium",          "#57178f",  55},
    {"Ba", "Barium",           "#00c900",  56},
    {"La", "Lanthanum",        "#70d4ff",  57},
    {"Ce", "Cerium",           "#ffffc7",  58},
    {"Pr", "Praseodymium",     "#d9ffc7",  59},
    {"Nd", "Neodymium",        "#c7ffc7",  60},
    {"Pm", "Promethium",       "#a3ffc7",  61},
    {"Sm", "Samarium",         "#8fffc7",  62},
    {"Eu", "Europium",         "#61ffc7",  63},
    {"Gd", "Gadolinium",       "#45ffc7",  64},
    {"Tb", "Terbium",          "#30ffc7",  65},
    {"Dy", "Dysprosium",       "#1fffc7",  66},
    {"Ho", "Holmium",          "#00ff9c",  67},
    {"Er", "Erbium",           "#00e675",  68},
    {"Tm", "Thulium",          "#00d452",  69},
    {"Yb", "Ytterbium",        "#00bf38",  70},
    {"Lu", "Lutetium",         "#00ab24",  71},
    {"Hf", "Hafnium",          "#4dc2ff",  72},
    {"Ta", "Tantalum",         "#4da6ff",  73},
    {"W",  "Tungsten",         "#2194d6",  74},
    {"Re", "Rhenium",          "#267dab",  75},
    {"Os", "Osmium",           "#266696",  76},
    {"Ir", "Iridium",          "#175487",  77},
    {"Pt", "Platinum",         "#d0d0e0",  78},
    {"Au", "Gold",             "#ffd123",  79},
    {"Hg", "Mercury",          "#b8b8d0",  80},
    {"Tl", "Thalium",          "#a6544d",  81},
    {"Pb", "Lead",             "#575961",  82},
    {"Bi", "Bismuth",          "#9e4fb5",  83},
    {"Po", "Polonium",         "#ab5c00",  84},
    {"At", "Astatine",         "#754f45",  85},
    {"Rn", "Radon",            "#428296",  86},
    {"Fr", "Francium",         "#420066",  87},
    {"Ra", "Radium",           "#007d00",  88},
    {"Ac", "Actinium",         "#70abfa",  89},
    {"Th", "Thorium",          "#00baff",  90},
    {"Pa", "Protactinium",     "#00a1ff",  91},
    {"U",  "Uranium",          "#008fff",  92},
    {"Np", "Neptunium",        "#0080ff",  93},
    {"Pu", "Plutonium",        "#006bff",  94},
    {"Am", "Americium",        "#545cf2",  95},
    {"Cm", "Curium",           "#785ce3",  96},
    {"Bk", "Berkelium",        "#8a4fe3",  97},
    {"Cf", "Californium",      "#a136d4",  98},
    {"Es", "Einsteinium",      "#b31fd4",  99},
    {"Fm", "Fermium",          "#b31fba", 100},
    {"Md", "Mendelevium",      "#b30da6", 101},
    {"No", "Nobelium",         "#bd0d87", 102},
    {"Lr", "Lawrencium",       "#c70066", 103},
    {"Rf", "Rutherfordium",    "#cc0059", 104},
    {"Db", "Dubnium",          "#d1004f", 105},
    {"Sg", "Seaborgium",       "#d90045", 106},
    {"Bh", "Bohrium",          "#e00038", 107},
    {"Hs", "Hassium",          "#e6002e", 108},
    {"Mt", "Meitnerium",       "#eb0026", 109},
    {"Ds", "Darmstadtium",     "#ee0023", 110},
    {"Rg", "Roentgenium",      "#fa0028", 111}
};

static JavAmino JAV_AMINO[] = {
    {" Ala", "A", "Alanine",                  "#ff0000"},
    {" Arg", "R", "Arginine",                 "#0000ff"},
    {" Asn", "N", "Asparagine",               "#00ff00"},
    {" Asp", "D", "Aspartic acid",            "#ffff00"},
    {" Cys", "C", "Cysteine",                 "#ff00ff"},
    {" Gln", "Q", "Glutamine",                "#00ffff"},
    {" Glu", "E", "Glutamic acid",            "#ffc0c0"},
    {" Gly", "G", "Glycine",                  "#00c0c0"},
    {" His", "H", "Histidine",                "#a600a6"},
    {" Ile", "I", "Isoleucine",               "#337fff"},
    {" Leu", "L", "Leucine",                  "#ff7f00"},
    {" Lys", "K", "Lysine",                   "#009900"},
    {" Met", "M", "Methionine",               "#0000b4"},
    {" Phe", "F", "Phenylalanine",            "#b40000"},
    {" Pro", "P", "Proline",                  "#59ccff"},
    {" Ser", "S", "Serine",                   "#cccccc"},
    {" Thr", "T", "Threonine",                "#bea06e"},
    {" Trp", "W", "Tryptophan",               "#ff8cff"},
    {" Tyr", "Y", "Tyrosine",                 "#ff7042"},
    {" Val", "V", "Valine",                   "#8282d2"},
    {" Mse", "M", "Selenomethionine",         "#0000b4"}
};

static JavAmino JAV_DNA_NUCLEOTIDES[] = {
    {"  DA", "A", "Adenine",                  "#ffff00"},
    {"  DC", "C", "Cytosine",                 "#eea612"},
    {"  DG", "G", "Guanine",                  "#bf0000"},
    {"  DT", "T", "Thymine",                  "#00bf00"},
    {"   U", "U", "Uracil",                   "#00bf00"}
};

static JavChain JAV_CHAIN[] = {
    {" A, a",  "#c0d0ff", },
    {" B, b",  "#b0ffb0", },
    {" C, c",  "#ffc0c8", },
    {" D, d",  "#ffff80", },
    {" E, e",  "#ffc0ff", },
    {" F, f",  "#b0f0f0", },
    {" G, g",  "#ffd070", },
    {" H, h",  "#f08080", },
    {" I, h",  "#f5deb3", },
    {" J, j",  "#00bfff", },
    {" K, k",  "#cd5c5c", },
    {" L, l",  "#66cdaa", },
    {" M, m",  "#9acd32", },
    {" N, n",  "#ee82ee", },
    {" O, o",  "#00ced1", },
    {" P, p",  "#00ff7f", },
    {" Q, q",  "#3cb371", },
    {" R, r",  "#00008b", },
    {" S, s",  "#bdb76b", },
    {" T, t",  "#006400", },
    {" U, u",  "#800000", },
    {" V, v",  "#808000", },
    {" W, w",  "#800080", },
    {" X, x",  "#008080", },
    {" Y, y",  "#b8860b", },
    {" Z, z",  "#b22222", }
 };

 static JavStructure JAV_STRUCTURE[] = {
     {"helix",    "#ff0000", },
     {"strand",   "#ffff00", },
     {"turn",     "#0000ff", },
     {"coil",     "#00ff00", },
     {"other",    "#ff69b4", }
 };

const int N_TRUE_ELEMENTS = 111;
const int N_AMINO = (int) (sizeof(JAV_AMINO)/sizeof(JavAmino));
const int N_DNA_NUCLEOTIDES = (int) (sizeof(JAV_DNA_NUCLEOTIDES)/sizeof(JavAmino));
const int N_CHAINS = (int) (sizeof(JAV_CHAIN)/sizeof(JavChain));
const int N_STRUCT  = (int) (sizeof(JAV_STRUCTURE)/sizeof(JavStructure));
const int NUM_COLORS = (int) (sizeof(JAV_CHAIN)/sizeof(JavChain) +
                              sizeof(JAV_STRUCTURE)/sizeof(JavStructure) +
                              sizeof(JAV_AMINO)/sizeof(JavAmino) +
                              sizeof(JAV_DNA_NUCLEOTIDES)/sizeof(JavAmino) +
                              sizeof(JAV_ELEMENTY)/sizeof(JavElement)) + 1;

typedef float atom_color[4];

typedef struct {
    int op;        // symmetry operator
    int tra[3];    // translation
               } op_type;

typedef struct {
      float xc[3];   // x,y,z
      float biso;    // fattore termico
      float bij[6];  // fattori termici anisotropi
      float och;     // occupanza chimica
      float ocry;    // occupanza cristall.
      float inte;    // intensita' di picco
      int nz;        // nz
      char lab[16];  // label
      int rcod[5];   // posizione nella matrice jac. del parametro se affinato; 0 se non affinato
      int ptab;      // pointer to table of elements
      float xsd[3];  // standard deviation on x,y,z
      float bsd;     // standard deviation on biso
      float osd;     // standard deviation on och
      int   asym;    // corrisponding atom in the a.u.
      op_type  op;   // symmetry operator
      int doc;       // dynamical occupancy correction
      char chain[14];// info about protein chain
                }  atom_type;

typedef struct {
               int   n1, n2;  // atomi legati
               float dist;    // distanza corrente
               float sigma;   // sigma
               int   ord;     // bond order
               } f_bond_type;

typedef struct {
     int    hkl[3];  // indici hkl
     int    m;       // molteplicità del riflesso
     float  tthd[2]; // 2-theta del riflesso in gradi
     float  slaq;    // ((sen(theta) / lambda))**2 = rho**2
     float  fo;      // fattori di struttura osservati
     float  fc;      // fattori di struttura calcolati dal modello
     int    ph;      // fasi calcolate dal modello
     float  fv;      // fattori di struttura veri
     int    phv;     // fase vera
     float  lp[2];   // correzione di Lorentz-polarizzazione
     float  as[2];   // correzione di assorbimento
     float  fwhm[2]; // fwhm
     float  po;      // P.O. correction
     float  pk;      // peak range
     float  rapI;    // intensity ratio
     int    jcode;   // code reflection
     int    rcod;    // refinement code for reflection
               } reflection_type;

typedef struct {
      int                        z             ;// atomic number Z
      char                       name[20]      ;// name
      char                       lab[8]        ;// label
      float                      weight        ;// atomic weight
      float                      c_radius      ;// covalent radius
      float                      w_radius      ;// van der Waals radius
      float                      ax[4]         ;// coefficients a per calcolo scattering factors (x-ray)
      float                      bx[4]         ;// coefficients b per calcolo scattering factors (x-ray)
      float                      cx            ;// coefficients c per calcolo scattering factors (x-ray)
      float                      fMo[2]        ;// f',f''
      float                      fCu[2]        ;// f',f''
      float                      f1,f2         ;// f',f'' at specified wavelength
      float                      ae[4]         ;// coefficients a per calcolo scattering factors (electron)
      float                      be[4]         ;// coefficients b per calcolo scattering factors (electron)
      float                      ce            ;// coefficients c per calcolo scattering factors (electron)
      float                      fact          ;// fattore di scattering dei neutroni
      double                     factE[12]     ;
      float                      al[4]         ;// used scattering factors
      float                      bs[4]         ;// used scattering factors
      float                      cl            ;// used scattering factors
      float                      nw            ;// number of elements
      float                      mac           ;// Mass attenuation coefficients (cm2 g-1)
      int                        radtype       ;// tipo di radiazione usata
      int                        charge        ;// charge
      int                        ptab          ;// pointer to table of elements
      float                      zeff;
      float                      ifMottFormula;
               } element_type;

typedef struct {
    int                 nspec;
    int                 zspec[MAX_SPECIES],vspec[MAX_SPECIES];
    float               cell[6];
    char                spaceg[16];
    float               cell_volume;
    float               volume_per_atom;
    float               r_factor;
    int                 icent;
    int                 nsymmstr;
               }  structure_info_type;

typedef struct {
                QColor light_chain;
                QColor labelcolor;
                QColor light_diffuse[NUM_COLORS];
                int LabelS[N_ELEMENTS];
               } statusSav;

enum plotstyles {WIRES, SIMPLE, BONDSTYLE, RODS, STICK_AND_BALLS, ELLIPSOIDS,
                 VANDERWAALS, POLYPEPTIDE, C_ALPHA, RIBBONS, STRANDS,
                 CARTOONS, PLOT_STYLES};
enum colormodes {BY_SPECIES, BY_B, BY_SOF, BY_SYMMETRY, MONOC, BY_RESIDUE,
                  BY_CHAIN, BY_STRUCT, BY_GROUP, COLOR_MODES};
enum glasses_types {REDBLUE, REDGREEN, REDCYAN, BLUERED, GREENRED, CYANRED, GLASSES_TYPES};
enum popup_types {ATOMS_POPUP, BONDS_POPUP, CALPHA_POPUP, BB_POPUP, LIG_POPUP, RESID_POPUP, LEGEND_POPUP};

typedef struct {
                 int   nsymop, latt, icent;
                 char  spg[17];
               }_SymmInfo;

typedef struct {
                  int op;
                  int wM[4], wA[4];
                  float result;
               }  DistAng_type;

typedef struct {
                 bool enabled;
                 QColor diffuse;
                 QColor specular;
                 float position[4];
               } _LightInfo;

Q_DECLARE_METATYPE(_LightInfo);


QColor get_gen_color(int num);
int ZFromPtab(int ptab);
#if 0
#ifdef __GNUC__
#if ((__GNUC__ >=4 ) && (__GNUC_MINOR__ >= 1))
// left-trim
static inline std::string &ltrim(std::string &s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](int c) {return !std::isspace(c);}));
    return s;
}
//static inline std::string &ltrim(std::string &s) {
//        s.erase(s.begin(), std::find_if(s.begin(), s.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
//        return s;
//}

// right-trim
static inline std::string &rtrim(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), [](int c) {return !std::isspace(c);}).base(), s.end());
    return s;
}
//static inline std::string &rtrim(std::string &s) {
//        s.erase(std::find_if(s.rbegin(), s.rend(), std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
//        return s;
//}

// left and right trim
static inline std::string &trim(std::string &s) {
        return ltrim(rtrim(s));
}
#else
static inline std::string &trim(std::string &s) {
// trim trailing spaces
   size_t endpos = s.find_last_not_of(" \t");
   if( std::string::npos != endpos )
   {
       s = s.substr( 0, endpos+1 );
   }
// trim leading spaces
   size_t startpos = s.find_first_not_of(" \t");
   if( std::string::npos != startpos )
   {
       s = s.substr( startpos );
   }
   return s;
}
#endif
#else
// left-trim
static inline std::string &ltrim(std::string &s) {
        s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char c) {return !::isspace(c);}));
        return s;
}
// right-trim
static inline std::string &rtrim(std::string &s) {
        s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char c) {return !::isspace(c);}).base(), s.end());
        return s;
}

// left and right trim
static inline std::string &trim(std::string &s) {
        return ltrim(rtrim(s));
}
#endif
#endif
// left-trim
static inline std::string &ltrim(std::string &s) {
        s.erase(s.begin(), std::find_if(s.begin(), s.end(), [](unsigned char c) {return !::isspace(c);}));
        return s;
}
// right-trim
static inline std::string &rtrim(std::string &s) {
        s.erase(std::find_if(s.rbegin(), s.rend(), [](unsigned char c) {return !::isspace(c);}).base(), s.end());
        return s;
}

// left and right trim
static inline std::string &trim(std::string &s) {
        return ltrim(rtrim(s));
}

static inline bool caseInsCharCompareN(char a, char b) {
     return(toupper(a) == toupper(b));
}

#define Is_Amino(x)      ((x)<=N_AMINO)
#define Is_Dna(x)        ((((x)>N_AMINO)) && ((x)<=N_AMINO+N_DNA_NUCLEOTIDES))
#define IsAdenine(x)     ((x)==N_AMINO+1)
#define IsCytosine(x)    ((x)==N_AMINO+2)
#define IsGuanine(x)     ((x)==N_AMINO+3)
#define IsThymine(x)     ((x)==N_AMINO+4)
#define IsUracil(x)      ((x)==N_AMINO+5)
#define IsPyrimidine(x)  (IsCytosine(x) || IsThymine(x) || IsUracil(x))
#define IsPurine(x)      (IsAdenine(x) || IsGuanine(x))
//#define IsPair(x,y)      ((x != y) && (((x+y) == 45) || ((x+y) == 46) || ((x+y) == 48)) )
//#define IsPair(x,y)      ((x != y) && (((x+y) == 45) || ((x+y) == 46)) )
#define IsPair(x,y)      ((x != y) && ((IsPurine(x) && IsPyrimidine(y)) || (IsPurine(y) && IsPyrimidine(x))) )
#define Is_Water(x)      ((x)==0)
#define AANonStd(x)      ((x)==500)
#define NANonStd(x)      ((x)==600)

inline bool Is_CNOS(int code)
{
    return(((code >= 5) && (code <= 7)) || (code == 15));
}

inline bool Is_AminoG(int code)
{
    return (Is_Amino(code) || AANonStd(code));
}

inline bool Is_DnaG(int code)
{
    return (Is_Dna(code) || NANonStd(code));
}

inline bool Is_Acid(int code)
{
    return (Is_DnaG(code) || Is_AminoG(code));
}

// Data extracted from a CIF file via Fortran get_crystal_info_from_cif
struct CifCrystalInfo {
    int   nat        = 0;
    int   zval       = 0;
    int   nrefl      = 0;
    int   nrefl_print = 0;
    float cellpar[6] = {};
    int   icell[6]   = {};
    float vol        = 0.f;
    float dens       = 0.f;
    float mu         = 0.f;
    float rir        = 0.f;
    float wavelen    = 0.f;
    char  sform[256] = {};
    char  subfile[32] = {};
    char  spg_sym[64] = {};
    char  crysys[64]  = {};
    int   refl_h[500]    = {};
    int   refl_k[500]    = {};
    int   refl_l[500]    = {};
    int   refl_mult[500] = {};
    float refl_tth[500]  = {};
    float refl_d[500]    = {};
    float refl_lp[500]   = {};
    float refl_fc2[500]  = {};
    float refl_inte[500] = {};
    float refl_ipct[500] = {};
    int   nelem          = 0;
    char  specie_label[100][3] = {};  // element symbols, null-terminated (2 chars + '\0')
    char  chem_name[256]    = {};
    char  mineral_name[256] = {};
};

// Calls Fortran get_crystal_info_from_cif for the given file.
// Returns true on success (ier == 0), false otherwise.
// If inorganicOnly is true, returns false when the structure is not inorganic
// (Fortran sets ier=1 in that case; caller should treat it as a skip, not an error).
bool readCrystalInfoFromCif(const QString &filePath, CifCrystalInfo &info, bool inorganicOnly = false);

// Initialises the Fortran chemical tables (load_chemical_tables + init_qualx).
// Must be called once before any CIF reading outside of qualxmain.
// Returns true on success.
bool initQualxTables(const QString &exePath);

void test_crystal_info_from_cif();
int leggixen(QWidget *w, QString file1);
bool caseInsCompare(const std::string &s1, const std::string &s2);
int is_valid_symbol(std::string el, int Ntot=0);
bool is_ghost(std::string el);
const char *get_nth_element(int num);
const char *get_nth_element_name(int num);
int IsAmino(const std::string &resname);
bool IsWater(const std::string &resname);
int IsDNA(const std::string &resname, const std::string &elem_name="");
int IsAminoNucle(const std::string &resname, const std::string &elem_name="");
const char *get_AminoCode(int num);
bool CheckAtom(const std::string &str);
bool IsProline(std::string &resname);
float Minimo (float vett[], int n);
double Minimo (const QVector<double> &vett);
float Massimo (float vett[], int n);
double Massimo (const QVector<double> &vett);
QColor get_elem_color(int num);
double CellVolume(const double cella[]);
double CellVolume(const float cella[]);
QString get_header(QString filnam);
QDataStream& operator<<(QDataStream& out, const _LightInfo& v);
QDataStream& operator>>(QDataStream& in, _LightInfo& v);
#endif
