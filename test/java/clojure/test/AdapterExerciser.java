
package clojure.test;

public class AdapterExerciser {
    @FunctionalInterface
    public interface L {
        public long takesRetL();
    }
    @FunctionalInterface
    public interface I {
        public int takesRetI();
    }
    @FunctionalInterface
    public interface S {
        public short takesRetS();
    }
    @FunctionalInterface
    public interface B {
        public byte takesRetB();
    }
    @FunctionalInterface
    public interface D {
        public double takesRetD();
    }
    @FunctionalInterface
    public interface F {
        public float takesRetF();
    }
    @FunctionalInterface
    public interface Z {
        public boolean takesRetZ();
    }
    @FunctionalInterface
    public interface O {
        public AdapterExerciser takesRetO();
    }
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
   public String methodL(L a) { return "L"; }
   public String methodI(I a) { return "I"; }
   public String methodS(S a) { return "S"; }
   public String methodB(B a) { return "B"; }
   public String methodD(D a) { return "D"; }
   public String methodF(F a) { return "F"; }
   public String methodZ(Z a) { return "Z"; }
   public String methodO(O a) { return "O"; }
   public String methodLL(LL a) { return "LL"; }
   public String methodDL(DL a) { return "DL"; }
   public String methodOL(OL a) { return "OL"; }
   public String methodLI(LI a) { return "LI"; }
   public String methodDI(DI a) { return "DI"; }
   public String methodOI(OI a) { return "OI"; }
   public String methodLS(LS a) { return "LS"; }
   public String methodDS(DS a) { return "DS"; }
   public String methodOS(OS a) { return "OS"; }
   public String methodLB(LB a) { return "LB"; }
   public String methodDB(DB a) { return "DB"; }
   public String methodOB(OB a) { return "OB"; }
   public String methodLD(LD a) { return "LD"; }
   public String methodDD(DD a) { return "DD"; }
   public String methodOD(OD a) { return "OD"; }
   public String methodLF(LF a) { return "LF"; }
   public String methodDF(DF a) { return "DF"; }
   public String methodOF(OF a) { return "OF"; }
   public String methodLZ(LZ a) { return "LZ"; }
   public String methodDZ(DZ a) { return "DZ"; }
   public String methodOZ(OZ a) { return "OZ"; }
   public String methodLO(LO a) { return "LO"; }
   public String methodDO(DO a) { return "DO"; }
   public String methodOO(OO a) { return "OO"; }
   public String methodLLL(LLL a) { return "LLL"; }
   public String methodLOL(LOL a) { return "LOL"; }
   public String methodOLL(OLL a) { return "OLL"; }
   public String methodDDL(DDL a) { return "DDL"; }
   public String methodLDL(LDL a) { return "LDL"; }
   public String methodDLL(DLL a) { return "DLL"; }
   public String methodOOL(OOL a) { return "OOL"; }
   public String methodODL(ODL a) { return "ODL"; }
   public String methodDOL(DOL a) { return "DOL"; }
   public String methodLLI(LLI a) { return "LLI"; }
   public String methodLOI(LOI a) { return "LOI"; }
   public String methodOLI(OLI a) { return "OLI"; }
   public String methodDDI(DDI a) { return "DDI"; }
   public String methodLDI(LDI a) { return "LDI"; }
   public String methodDLI(DLI a) { return "DLI"; }
   public String methodOOI(OOI a) { return "OOI"; }
   public String methodODI(ODI a) { return "ODI"; }
   public String methodDOI(DOI a) { return "DOI"; }
   public String methodLLS(LLS a) { return "LLS"; }
   public String methodLOS(LOS a) { return "LOS"; }
   public String methodOLS(OLS a) { return "OLS"; }
   public String methodDDS(DDS a) { return "DDS"; }
   public String methodLDS(LDS a) { return "LDS"; }
   public String methodDLS(DLS a) { return "DLS"; }
   public String methodOOS(OOS a) { return "OOS"; }
   public String methodODS(ODS a) { return "ODS"; }
   public String methodDOS(DOS a) { return "DOS"; }
   public String methodLLB(LLB a) { return "LLB"; }
   public String methodLOB(LOB a) { return "LOB"; }
   public String methodOLB(OLB a) { return "OLB"; }
   public String methodDDB(DDB a) { return "DDB"; }
   public String methodLDB(LDB a) { return "LDB"; }
   public String methodDLB(DLB a) { return "DLB"; }
   public String methodOOB(OOB a) { return "OOB"; }
   public String methodODB(ODB a) { return "ODB"; }
   public String methodDOB(DOB a) { return "DOB"; }
   public String methodLLD(LLD a) { return "LLD"; }
   public String methodLOD(LOD a) { return "LOD"; }
   public String methodOLD(OLD a) { return "OLD"; }
   public String methodDDD(DDD a) { return "DDD"; }
   public String methodLDD(LDD a) { return "LDD"; }
   public String methodDLD(DLD a) { return "DLD"; }
   public String methodOOD(OOD a) { return "OOD"; }
   public String methodODD(ODD a) { return "ODD"; }
   public String methodDOD(DOD a) { return "DOD"; }
   public String methodLLF(LLF a) { return "LLF"; }
   public String methodLOF(LOF a) { return "LOF"; }
   public String methodOLF(OLF a) { return "OLF"; }
   public String methodDDF(DDF a) { return "DDF"; }
   public String methodLDF(LDF a) { return "LDF"; }
   public String methodDLF(DLF a) { return "DLF"; }
   public String methodOOF(OOF a) { return "OOF"; }
   public String methodODF(ODF a) { return "ODF"; }
   public String methodDOF(DOF a) { return "DOF"; }
   public String methodLLZ(LLZ a) { return "LLZ"; }
   public String methodLOZ(LOZ a) { return "LOZ"; }
   public String methodOLZ(OLZ a) { return "OLZ"; }
   public String methodDDZ(DDZ a) { return "DDZ"; }
   public String methodLDZ(LDZ a) { return "LDZ"; }
   public String methodDLZ(DLZ a) { return "DLZ"; }
   public String methodOOZ(OOZ a) { return "OOZ"; }
   public String methodODZ(ODZ a) { return "ODZ"; }
   public String methodDOZ(DOZ a) { return "DOZ"; }
   public String methodLLO(LLO a) { return "LLO"; }
   public String methodLOO(LOO a) { return "LOO"; }
   public String methodOLO(OLO a) { return "OLO"; }
   public String methodDDO(DDO a) { return "DDO"; }
   public String methodLDO(LDO a) { return "LDO"; }
   public String methodDLO(DLO a) { return "DLO"; }
   public String methodOOO(OOO a) { return "OOO"; }
   public String methodODO(ODO a) { return "ODO"; }
   public String methodDOO(DOO a) { return "DOO"; }
   public String methodOOOZ(OOOZ a) { return "OOOZ"; }
   public String methodOOOO(OOOO a) { return "OOOO"; }
   public String methodOOOOZ(OOOOZ a) { return "OOOOZ"; }
   public String methodOOOOO(OOOOO a) { return "OOOOO"; }
   public String methodOOOOOZ(OOOOOZ a) { return "OOOOOZ"; }
   public String methodOOOOOO(OOOOOO a) { return "OOOOOO"; }
   public String methodOOOOOOZ(OOOOOOZ a) { return "OOOOOOZ"; }
   public String methodOOOOOOO(OOOOOOO a) { return "OOOOOOO"; }
   public String methodOOOOOOOZ(OOOOOOOZ a) { return "OOOOOOOZ"; }
   public String methodOOOOOOOO(OOOOOOOO a) { return "OOOOOOOO"; }
   public String methodOOOOOOOOZ(OOOOOOOOZ a) { return "OOOOOOOOZ"; }
   public String methodOOOOOOOOO(OOOOOOOOO a) { return "OOOOOOOOO"; }
   public String methodOOOOOOOOOZ(OOOOOOOOOZ a) { return "OOOOOOOOOZ"; }
   public String methodOOOOOOOOOO(OOOOOOOOOO a) { return "OOOOOOOOOO"; }
   public String methodOOOOOOOOOOZ(OOOOOOOOOOZ a) { return "OOOOOOOOOOZ"; }
   public String methodOOOOOOOOOOO(OOOOOOOOOOO a) { return "OOOOOOOOOOO"; }}