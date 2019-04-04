import java.util.HashSet;

import com.franz.jlinker.JLinkerPortException;


// Small example on how to use the SNePSAPI

// Copy to your own directory if you want to recompile the code.
// Replace "/projects/snwiz/Install/Sneps-2.7.0" with the location of the
// SNePS directory


// Compile: javac -classpath ".:/projects/snwiz/Install/Sneps-2.7.0/Jlinker/jlinker.jar:/projects/snwiz/Install/Sneps-2.7.0/JavaSnepsAPI/JavaSnepsAPI.jar" TestAPI.java

// My version compiled under Java 1.5
// Run: java -classpath ".:/projects/snwiz/Install/Sneps-2.7.0/Jlinker/jlinker.jar:/projects/snwiz/Install/Sneps-2.7.0/JavaSnepsAPI/JavaSnepsAPI.jar" TestAPI

public class TestAPI {
	static JavaSnepsAPI api;

    
    public static String [] commands
	= {"set-mode-3",
	   "define-frame Isa (nil member class)",
	   "define-frame Ako (nil class1 class2)",
	   "define-frame Believes (nil agent prop)",
	   "Isa(Clyde, Elephant).",
	   "Isa(Dumbo, Elephant).",
	   "Isa(Tweety, Canary).",
	   "Ako(Elephant, Mammal).",
	   //"Ako(Mammal, Animal).",
	   //"Ako(Bird, Animal)."
	   "all(x)(Isa(x, Elephant) => Isa(x, Mammal)).",
	   "all(x)(Isa(x, Mammal) => Isa(x, Animal))."};
    
    static String [] KB={
    	"set-mode-3",
	    "define-frame g (nil prop grade)",
	    "define-frame dur (r a1)",
	    "define-frame Holds (r a1 a2)",
	    "define-frame incDur (action object1)", 
	    "^^",
	    "(define-primaction incDur (object1)(jcall (jmethod \"TestAPI\" \"testJava2\" \"java.lang.String\") (jclass \"TestAPI\") object1))", 
	    "(attach-primaction incDur incDur)",
	    "^^",
	    "perform incDur(2)"};
    
    public static String [] commands1
	= {"set-mode-1",
	   "all(x)(Frog(x) => Green(x)).",
	   "Croaks(Froggie).",
	   "Sings(Tweety).",
	   "all(x)(Canary(x) => Yellow(x)).",
	   "all(x)(Croaks(x) => Frog(x)).",
	   "all(x)(Sings(x) => Canary(x))."
	  };
    
    public static String [] commands2
   	= {"set-mode-1",
   	   "man(john).",
   	   "woman(mary).",
   	   "married(john,mary).",
   	   "all(x,y)((man(x) and woman(y) and married(x,y))=> husband(x,y))."
   	  };
    
    public static void testJava()
    {
    	System.out.println("It worked!");
    }
    
    public static void testJava2(String n) throws JLinkerPortException
    {
    	String s1= n.replace("(", "");
    	s1= s1.replace(")", "");
    	String s= "dur("+(Integer.parseInt(s1)+1)+").";
    	api.tell(s);
    }

    
    public static void main(String[] args) throws JLinkerPortException{
       
	// Create an instance of the API
	//JavaSnepsAPI api = new JavaSnepsAPI(3179);
     api = new JavaSnepsAPI(3179);
	// Testing the tell method:
	for (String cmd: KB) {
	    System.out.println("Telling: " + cmd);
	    api.tell(cmd);
	} 

	// Testing the ask method:
	//System.out.println("Asking Isa(?x, Mammal)...");
	//HashSet<String> answers = api.ask("Husband(?x,?y)");

	//for(String answer: answers) {
	//    System.out.println("Received:" + answer);
	//} 
     
//	HashSet<String> answers1 = api.ask("Yellow(?x)");
//
//	for(String answer: answers1) {
//	    System.out.println("Received:" + answer);
//	} 
       
	// Testing the askwh method:
	/*System.out.println("Asking Ako(?x,?y)...");
	HashSet<Substitution> substset = api.askwh("Ako(?x,?y)");
	for(Substitution subst: substset) {
	    System.out.println("Received: " + subst.getValFromVar("x")
			       + "s are " + subst.getValFromVar("y") + "s.");
	}*/
	//api.endLispConnection();

    }
}
//(load "/home/nour/Sneps-2.7.0/load-sneps.lisp")
//(SNEPSLOG:INIT-JAVA-SNEPS-CONNECTION 3179 "/home/nour/workspace/Masters/bin:/home/nour/acl90express/jlinker/jlinker.jar:/home/nour/test2.jar")
