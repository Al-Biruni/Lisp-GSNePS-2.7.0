package edu.buffalo.sneps;

/**
 * Represents a substitution (a list of variables and the values that unify with them). 
 * Used by the {@link edu.buffalo.sneps.JavaSnepsAPI#askwh(String) askwh} and {@link edu.buffalo.sneps.JavaSnepsAPI#askwhnot(String) askwhnot} methods to represent substitutions. An array list of these are returned by these methods as there can be multiple substitutions that satisfy a particular query.
 **/
public class Substitution {

    /**
     * Array of substitution pairs that make up this substitution
     **/
    private SubstitutionPair [] subs;

    /**
     * Number of pairs in the substitution
     **/
    private int count = 0;
	
    /**
     * Creates and instance of the substitution with the specified capacity (number of substitution pairs).
     * @param capacity The capacity for this substitution
     **/
    public Substitution (int capacity) {
	subs = new SubstitutionPair [capacity];
    }

    /**
     * Returns the number of Substitutions in this set.
     * @return The number of substitutions in the set
     **/
    public int size (){
	return count;
    }

    /**
     * Adds a substitution pair to this substitution of the specified variable and value.
     * @param var The variable identifier.
     * @param val The value that unifies with that variable
     **/
    public void addSubstitution(String var, String val) {
	subs[count++] = new SubstitutionPair(var,val);
    }

    /** Returns the value that unifies with the specified variable in this substitution.
     * @param var The variable identifier to retrieve the value from.
     * @return The value that unifies with <code>var</code>, or the empty string is no such variable is found in this set.
     **/
    public String getValFromVar (String var) {
	//System.out.println("Got: " + var_val + ";Looking for: " + var);
	SubstitutionPair elm;
	for (int i = 0; i < subs.length; i++){
	    elm = subs[i];
	    if(var.equals(elm.variable)){
		return elm.value;
	    }
	}
	return "";
    }

    public String toString () {
	String result = "(";
	for(int i = 0; i < count; i++)
	    result = result +  subs[i] + " ";
	result = result + ")";
	return result;
    }
	
    /** 
     * Represents an instance of a variable replacement 
     * (a variable and the value it unifies with).
     **/
    private class SubstitutionPair {
	    
	/**
	 * The variable in this substitution
	 **/
	private String variable = "";

	/** 
	 * The value in this substitution
	 **/
	private String value = "";
	    
	/**
	 * Creates a substitution pair out of the given variable and value.
	 * @param var The variable.
	 * @param val The value.
	 **/
	public SubstitutionPair (String var, String val) {
	    variable = var;
	    value = val;
	}

	    
	public String toString () {
	    return "(" + variable + " . " + value + ")";
	}
	    
    }
}