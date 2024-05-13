package clojure.test;

public class AdapterExerciser {
    @FunctionalInterface
    public interface LL {
        public long takesLRetL(long a);
    }
    @FunctionalInterface
    public interface DL {
        public long takesDRetL(double a);
    }
    @FunctionalInterface
    public interface OL {
        public long takesORetL(AdapterExerciser a);
    }
    @FunctionalInterface
    public interface LI {
        public int takesLRetI(long a);
    }
    @FunctionalInterface
    public interface DI {
        public int takesDRetI(double a);
    }
    @FunctionalInterface
    public interface OI {
        public int takesORetI(AdapterExerciser a);
    }
    @FunctionalInterface
    public interface LS {
        public short takesLRetS(long a);
    }
    @FunctionalInterface
    public interface DS {
        public short takesDRetS(double a);
    }
    @FunctionalInterface
    public interface OS {
        public short takesORetS(AdapterExerciser a);
    }
    @FunctionalInterface
    public interface LB {
        public byte takesLRetB(long a);
    }
    @FunctionalInterface
    public interface DB {
        public byte takesDRetB(double a);
    }
    @FunctionalInterface
    public interface OB {
        public byte takesORetB(AdapterExerciser a);
    }
    @FunctionalInterface
    public interface LD {
        public double takesLRetD(long a);
    }
    @FunctionalInterface
    public interface DD {
        public double takesDRetD(double a);
    }
    @FunctionalInterface
    public interface OD {
        public double takesORetD(AdapterExerciser a);
    }
    @FunctionalInterface
    public interface LF {
        public float takesLRetF(long a);
    }
    @FunctionalInterface
    public interface DF {
        public float takesDRetF(double a);
    }
    @FunctionalInterface
    public interface OF {
        public float takesORetF(AdapterExerciser a);
    }
    @FunctionalInterface
    public interface LZ {
        public boolean takesLRetZ(long a);
    }
    @FunctionalInterface
    public interface DZ {
        public boolean takesDRetZ(double a);
    }
    @FunctionalInterface
    public interface OZ {
        public boolean takesORetZ(AdapterExerciser a);
    }
    @FunctionalInterface
    public interface LO {
        public AdapterExerciser takesLRetO(long a);
    }
    @FunctionalInterface
    public interface DO {
        public AdapterExerciser takesDRetO(double a);
    }
    @FunctionalInterface
    public interface OO {
        public AdapterExerciser takesORetO(AdapterExerciser a);
    }
    @FunctionalInterface
    public interface LLL {
        public long takesLLRetL(long a, long b);
    }
    @FunctionalInterface
    public interface LOL {
        public long takesLORetL(long a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface OLL {
        public long takesOLRetL(AdapterExerciser a, long b);
    }
    @FunctionalInterface
    public interface DDL {
        public long takesDDRetL(double a, double b);
    }
    @FunctionalInterface
    public interface LDL {
        public long takesLDRetL(long a, double b);
    }
    @FunctionalInterface
    public interface DLL {
        public long takesDLRetL(double a, long b);
    }
    @FunctionalInterface
    public interface OOL {
        public long takesOORetL(AdapterExerciser a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface ODL {
        public long takesODRetL(AdapterExerciser a, double b);
    }
    @FunctionalInterface
    public interface DOL {
        public long takesDORetL(double a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface LLI {
        public int takesLLRetI(long a, long b);
    }
    @FunctionalInterface
    public interface LOI {
        public int takesLORetI(long a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface OLI {
        public int takesOLRetI(AdapterExerciser a, long b);
    }
    @FunctionalInterface
    public interface DDI {
        public int takesDDRetI(double a, double b);
    }
    @FunctionalInterface
    public interface LDI {
        public int takesLDRetI(long a, double b);
    }
    @FunctionalInterface
    public interface DLI {
        public int takesDLRetI(double a, long b);
    }
    @FunctionalInterface
    public interface OOI {
        public int takesOORetI(AdapterExerciser a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface ODI {
        public int takesODRetI(AdapterExerciser a, double b);
    }
    @FunctionalInterface
    public interface DOI {
        public int takesDORetI(double a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface LLS {
        public short takesLLRetS(long a, long b);
    }
    @FunctionalInterface
    public interface LOS {
        public short takesLORetS(long a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface OLS {
        public short takesOLRetS(AdapterExerciser a, long b);
    }
    @FunctionalInterface
    public interface DDS {
        public short takesDDRetS(double a, double b);
    }
    @FunctionalInterface
    public interface LDS {
        public short takesLDRetS(long a, double b);
    }
    @FunctionalInterface
    public interface DLS {
        public short takesDLRetS(double a, long b);
    }
    @FunctionalInterface
    public interface OOS {
        public short takesOORetS(AdapterExerciser a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface ODS {
        public short takesODRetS(AdapterExerciser a, double b);
    }
    @FunctionalInterface
    public interface DOS {
        public short takesDORetS(double a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface LLB {
        public byte takesLLRetB(long a, long b);
    }
    @FunctionalInterface
    public interface LOB {
        public byte takesLORetB(long a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface OLB {
        public byte takesOLRetB(AdapterExerciser a, long b);
    }
    @FunctionalInterface
    public interface DDB {
        public byte takesDDRetB(double a, double b);
    }
    @FunctionalInterface
    public interface LDB {
        public byte takesLDRetB(long a, double b);
    }
    @FunctionalInterface
    public interface DLB {
        public byte takesDLRetB(double a, long b);
    }
    @FunctionalInterface
    public interface OOB {
        public byte takesOORetB(AdapterExerciser a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface ODB {
        public byte takesODRetB(AdapterExerciser a, double b);
    }
    @FunctionalInterface
    public interface DOB {
        public byte takesDORetB(double a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface LLD {
        public double takesLLRetD(long a, long b);
    }
    @FunctionalInterface
    public interface LOD {
        public double takesLORetD(long a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface OLD {
        public double takesOLRetD(AdapterExerciser a, long b);
    }
    @FunctionalInterface
    public interface DDD {
        public double takesDDRetD(double a, double b);
    }
    @FunctionalInterface
    public interface LDD {
        public double takesLDRetD(long a, double b);
    }
    @FunctionalInterface
    public interface DLD {
        public double takesDLRetD(double a, long b);
    }
    @FunctionalInterface
    public interface OOD {
        public double takesOORetD(AdapterExerciser a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface ODD {
        public double takesODRetD(AdapterExerciser a, double b);
    }
    @FunctionalInterface
    public interface DOD {
        public double takesDORetD(double a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface LLF {
        public float takesLLRetF(long a, long b);
    }
    @FunctionalInterface
    public interface LOF {
        public float takesLORetF(long a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface OLF {
        public float takesOLRetF(AdapterExerciser a, long b);
    }
    @FunctionalInterface
    public interface DDF {
        public float takesDDRetF(double a, double b);
    }
    @FunctionalInterface
    public interface LDF {
        public float takesLDRetF(long a, double b);
    }
    @FunctionalInterface
    public interface DLF {
        public float takesDLRetF(double a, long b);
    }
    @FunctionalInterface
    public interface OOF {
        public float takesOORetF(AdapterExerciser a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface ODF {
        public float takesODRetF(AdapterExerciser a, double b);
    }
    @FunctionalInterface
    public interface DOF {
        public float takesDORetF(double a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface LLZ {
        public boolean takesLLRetZ(long a, long b);
    }
    @FunctionalInterface
    public interface LOZ {
        public boolean takesLORetZ(long a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface OLZ {
        public boolean takesOLRetZ(AdapterExerciser a, long b);
    }
    @FunctionalInterface
    public interface DDZ {
        public boolean takesDDRetZ(double a, double b);
    }
    @FunctionalInterface
    public interface LDZ {
        public boolean takesLDRetZ(long a, double b);
    }
    @FunctionalInterface
    public interface DLZ {
        public boolean takesDLRetZ(double a, long b);
    }
    @FunctionalInterface
    public interface OOZ {
        public boolean takesOORetZ(AdapterExerciser a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface ODZ {
        public boolean takesODRetZ(AdapterExerciser a, double b);
    }
    @FunctionalInterface
    public interface DOZ {
        public boolean takesDORetZ(double a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface LLO {
        public AdapterExerciser takesLLRetO(long a, long b);
    }
    @FunctionalInterface
    public interface LOO {
        public AdapterExerciser takesLORetO(long a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface OLO {
        public AdapterExerciser takesOLRetO(AdapterExerciser a, long b);
    }
    @FunctionalInterface
    public interface DDO {
        public AdapterExerciser takesDDRetO(double a, double b);
    }
    @FunctionalInterface
    public interface LDO {
        public AdapterExerciser takesLDRetO(long a, double b);
    }
    @FunctionalInterface
    public interface DLO {
        public AdapterExerciser takesDLRetO(double a, long b);
    }
    @FunctionalInterface
    public interface OOO {
        public AdapterExerciser takesOORetO(AdapterExerciser a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface ODO {
        public AdapterExerciser takesODRetO(AdapterExerciser a, double b);
    }
    @FunctionalInterface
    public interface DOO {
        public AdapterExerciser takesDORetO(double a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface OOOZ {
        public boolean takesOOORetZ(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c);
    }
    @FunctionalInterface
    public interface OOOO {
        public AdapterExerciser takesOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c);
    }
    @FunctionalInterface
    public interface OOOOZ {
        public boolean takesOOOORetZ(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d);
    }
    @FunctionalInterface
    public interface OOOOO {
        public AdapterExerciser takesOOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d);
    }
    @FunctionalInterface
    public interface OOOOOZ {
        public boolean takesOOOOORetZ(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e);
    }
    @FunctionalInterface
    public interface OOOOOO {
        public AdapterExerciser takesOOOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e);
    }
    @FunctionalInterface
    public interface OOOOOOZ {
        public boolean takesOOOOOORetZ(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f);
    }
    @FunctionalInterface
    public interface OOOOOOO {
        public AdapterExerciser takesOOOOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f);
    }
    @FunctionalInterface
    public interface OOOOOOOZ {
        public boolean takesOOOOOOORetZ(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g);
    }
    @FunctionalInterface
    public interface OOOOOOOO {
        public AdapterExerciser takesOOOOOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g);
    }
    @FunctionalInterface
    public interface OOOOOOOOZ {
        public boolean takesOOOOOOOORetZ(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g, AdapterExerciser h);
    }
    @FunctionalInterface
    public interface OOOOOOOOO {
        public AdapterExerciser takesOOOOOOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g, AdapterExerciser h);
    }
    @FunctionalInterface
    public interface OOOOOOOOOZ {
        public boolean takesOOOOOOOOORetZ(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g, AdapterExerciser h, AdapterExerciser i);
    }
    @FunctionalInterface
    public interface OOOOOOOOOO {
        public AdapterExerciser takesOOOOOOOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g, AdapterExerciser h, AdapterExerciser i);
    }
    @FunctionalInterface
    public interface OOOOOOOOOOZ {
        public boolean takesOOOOOOOOOORetZ(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g, AdapterExerciser h, AdapterExerciser i, AdapterExerciser j);
    }
    @FunctionalInterface
    public interface OOOOOOOOOOO {
        public AdapterExerciser takesOOOOOOOOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g, AdapterExerciser h, AdapterExerciser i, AdapterExerciser j);
    }
    public int methodLL(LL a) { return 0; }
    public int methodDL(DL a) { return 1; }
    public int methodOL(OL a) { return 2; }
    public int methodLI(LI a) { return 3; }
    public int methodDI(DI a) { return 4; }
    public int methodOI(OI a) { return 5; }
    public int methodLS(LS a) { return 6; }
    public int methodDS(DS a) { return 7; }
    public int methodOS(OS a) { return 8; }
    public int methodLB(LB a) { return 9; }
    public int methodDB(DB a) { return 10; }
    public int methodOB(OB a) { return 11; }
    public int methodLD(LD a) { return 12; }
    public int methodDD(DD a) { return 13; }
    public int methodOD(OD a) { return 14; }
    public int methodLF(LF a) { return 15; }
    public int methodDF(DF a) { return 16; }
    public int methodOF(OF a) { return 17; }
    public int methodLZ(LZ a) { return 18; }
    public int methodDZ(DZ a) { return 19; }
    public int methodOZ(OZ a) { return 20; }
    public int methodLO(LO a) { return 21; }
    public int methodDO(DO a) { return 22; }
    public int methodOO(OO a) { return 23; }
    public int methodLLL(LLL a) { return 24; }
    public int methodLOL(LOL a) { return 25; }
    public int methodOLL(OLL a) { return 26; }
    public int methodDDL(DDL a) { return 27; }
    public int methodLDL(LDL a) { return 28; }
    public int methodDLL(DLL a) { return 29; }
    public int methodOOL(OOL a) { return 30; }
    public int methodODL(ODL a) { return 31; }
    public int methodDOL(DOL a) { return 32; }
    public int methodLLI(LLI a) { return 33; }
    public int methodLOI(LOI a) { return 34; }
    public int methodOLI(OLI a) { return 35; }
    public int methodDDI(DDI a) { return 36; }
    public int methodLDI(LDI a) { return 37; }
    public int methodDLI(DLI a) { return 38; }
    public int methodOOI(OOI a) { return 39; }
    public int methodODI(ODI a) { return 40; }
    public int methodDOI(DOI a) { return 41; }
    public int methodLLS(LLS a) { return 42; }
    public int methodLOS(LOS a) { return 43; }
    public int methodOLS(OLS a) { return 44; }
    public int methodDDS(DDS a) { return 45; }
    public int methodLDS(LDS a) { return 46; }
    public int methodDLS(DLS a) { return 47; }
    public int methodOOS(OOS a) { return 48; }
    public int methodODS(ODS a) { return 49; }
    public int methodDOS(DOS a) { return 50; }
    public int methodLLB(LLB a) { return 51; }
    public int methodLOB(LOB a) { return 52; }
    public int methodOLB(OLB a) { return 53; }
    public int methodDDB(DDB a) { return 54; }
    public int methodLDB(LDB a) { return 55; }
    public int methodDLB(DLB a) { return 56; }
    public int methodOOB(OOB a) { return 57; }
    public int methodODB(ODB a) { return 58; }
    public int methodDOB(DOB a) { return 59; }
    public int methodLLD(LLD a) { return 60; }
    public int methodLOD(LOD a) { return 61; }
    public int methodOLD(OLD a) { return 62; }
    public int methodDDD(DDD a) { return 63; }
    public int methodLDD(LDD a) { return 64; }
    public int methodDLD(DLD a) { return 65; }
    public int methodOOD(OOD a) { return 66; }
    public int methodODD(ODD a) { return 67; }
    public int methodDOD(DOD a) { return 68; }
    public int methodLLF(LLF a) { return 69; }
    public int methodLOF(LOF a) { return 70; }
    public int methodOLF(OLF a) { return 71; }
    public int methodDDF(DDF a) { return 72; }
    public int methodLDF(LDF a) { return 73; }
    public int methodDLF(DLF a) { return 74; }
    public int methodOOF(OOF a) { return 75; }
    public int methodODF(ODF a) { return 76; }
    public int methodDOF(DOF a) { return 77; }
    public int methodLLZ(LLZ a) { return 78; }
    public int methodLOZ(LOZ a) { return 79; }
    public int methodOLZ(OLZ a) { return 80; }
    public int methodDDZ(DDZ a) { return 81; }
    public int methodLDZ(LDZ a) { return 82; }
    public int methodDLZ(DLZ a) { return 83; }
    public int methodOOZ(OOZ a) { return 84; }
    public int methodODZ(ODZ a) { return 85; }
    public int methodDOZ(DOZ a) { return 86; }
    public int methodLLO(LLO a) { return 87; }
    public int methodLOO(LOO a) { return 88; }
    public int methodOLO(OLO a) { return 89; }
    public int methodDDO(DDO a) { return 90; }
    public int methodLDO(LDO a) { return 91; }
    public int methodDLO(DLO a) { return 92; }
    public int methodOOO(OOO a) { return 93; }
    public int methodODO(ODO a) { return 94; }
    public int methodDOO(DOO a) { return 95; }
    public int methodOOOZ(OOOZ a) { return 96; }
    public int methodOOOO(OOOO a) { return 97; }
    public int methodOOOOZ(OOOOZ a) { return 98; }
    public int methodOOOOO(OOOOO a) { return 99; }
    public int methodOOOOOZ(OOOOOZ a) { return 100; }
    public int methodOOOOOO(OOOOOO a) { return 101; }
    public int methodOOOOOOZ(OOOOOOZ a) { return 102; }
    public int methodOOOOOOO(OOOOOOO a) { return 103; }
    public int methodOOOOOOOZ(OOOOOOOZ a) { return 104; }
    public int methodOOOOOOOO(OOOOOOOO a) { return 105; }
    public int methodOOOOOOOOZ(OOOOOOOOZ a) { return 106; }
    public int methodOOOOOOOOO(OOOOOOOOO a) { return 107; }
    public int methodOOOOOOOOOZ(OOOOOOOOOZ a) { return 108; }
    public int methodOOOOOOOOOO(OOOOOOOOOO a) { return 109; }
    public int methodOOOOOOOOOOZ(OOOOOOOOOOZ a) { return 110; }
    public int methodOOOOOOOOOOO(OOOOOOOOOOO a) { return 111; }}
