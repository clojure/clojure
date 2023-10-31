package clojure.test;

public class AdapterExerciser {

    @FunctionalInterface
    public interface L {
        public long RetL();
    }
    @FunctionalInterface
    public interface I {
        public int RetI();
    }
    @FunctionalInterface
    public interface B {
        public boolean RetB();
    }
    @FunctionalInterface
    public interface D {
        public double RetD();
    }
    @FunctionalInterface
    public interface O {
        public AdapterExerciser RetO();
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
    public interface LB {
        public boolean takesLRetB(long a);
    }
    @FunctionalInterface
    public interface DB {
        public boolean takesDRetB(double a);
    }
    @FunctionalInterface
    public interface OB {
        public boolean takesORetB(AdapterExerciser a);
    }
    @FunctionalInterface
    public interface OD {
        public double takesORetD(AdapterExerciser a);
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
    public interface LLB {
        public boolean takesLLRetB(long a, long b);
    }
    @FunctionalInterface
    public interface LOB {
        public boolean takesLORetB(long a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface OLB {
        public boolean takesOLRetB(AdapterExerciser a, long b);
    }
    @FunctionalInterface
    public interface DDB {
        public boolean takesDDRetB(double a, double b);
    }
    @FunctionalInterface
    public interface LDB {
        public boolean takesLDRetB(long a, double b);
    }
    @FunctionalInterface
    public interface DLB {
        public boolean takesDLRetB(double a, long b);
    }
    @FunctionalInterface
    public interface OOB {
        public boolean takesOORetB(AdapterExerciser a, AdapterExerciser b);
    }
    @FunctionalInterface
    public interface ODB {
        public boolean takesODRetB(AdapterExerciser a, double b);
    }
    @FunctionalInterface
    public interface DOB {
        public boolean takesDORetB(double a, AdapterExerciser b);
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
    public interface OOOO {
        public AdapterExerciser takesOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c);
    }
    @FunctionalInterface
    public interface OOOOO {
        public AdapterExerciser takesOOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d);
    }
    @FunctionalInterface
    public interface OOOOOO {
        public AdapterExerciser takesOOOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e);
    }
    @FunctionalInterface
    public interface OOOOOOO {
        public AdapterExerciser takesOOOOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f);
    }
    @FunctionalInterface
    public interface OOOOOOOO {
        public AdapterExerciser takesOOOOOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g);
    }
    @FunctionalInterface
    public interface OOOOOOOOO {
        public AdapterExerciser takesOOOOOOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g, AdapterExerciser h);
    }
    @FunctionalInterface
    public interface OOOOOOOOOO {
        public AdapterExerciser takesOOOOOOOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g, AdapterExerciser h, AdapterExerciser i);
    }
    @FunctionalInterface
    public interface OOOOOOOOOOO {
        public AdapterExerciser takesOOOOOOOOOORetO(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g, AdapterExerciser h, AdapterExerciser i, AdapterExerciser j);
    }
    @FunctionalInterface
    public interface OOOB {
        public boolean takesOOORetB(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c);
    }
    @FunctionalInterface
    public interface OOOOB {
        public boolean takesOOOORetB(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d);
    }
    @FunctionalInterface
    public interface OOOOOB {
        public boolean takesOOOOORetB(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e);
    }
    @FunctionalInterface
    public interface OOOOOOB {
        public boolean takesOOOOOORetB(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f);
    }
    @FunctionalInterface
    public interface OOOOOOOB {
        public boolean takesOOOOOOORetB(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g);
    }
    @FunctionalInterface
    public interface OOOOOOOOB {
        public boolean takesOOOOOOOORetB(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g, AdapterExerciser h);
    }
    @FunctionalInterface
    public interface OOOOOOOOOB {
        public boolean takesOOOOOOOOORetB(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g, AdapterExerciser h, AdapterExerciser i);
    }
    @FunctionalInterface
    public interface OOOOOOOOOOB {
        public boolean takesOOOOOOOOOORetB(AdapterExerciser a, AdapterExerciser b, AdapterExerciser c, AdapterExerciser d, AdapterExerciser e, AdapterExerciser f, AdapterExerciser g, AdapterExerciser h, AdapterExerciser i, AdapterExerciser j);
    }
}
