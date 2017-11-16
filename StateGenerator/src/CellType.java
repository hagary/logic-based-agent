import java.util.EnumSet;


public enum CellType {
	BLANK,
	OBSTACLE,
	ROCK,
	PAD,
	ROCKONPAD,
	TELEPORTAL,
	ROCKONTELEPORTAL;

	public static EnumSet<CellType> getRockTypes(){
		return EnumSet.of(ROCK, ROCKONPAD, ROCKONTELEPORTAL);
	}
	public static EnumSet<CellType> getPadTypes(){
		return EnumSet.of(PAD, ROCKONPAD);
	}
	public static CellType getType(String s){
		/*
		 * Returns the type (as enum) of the given cell notation.
		 */
		for(CellType t : CellType.values()){
			if(t.toString().equals(s))
				return t;
		}
		return null;
	}
	public String toString(){
		switch(this){
		case BLANK: return "B";
		case OBSTACLE: return "O";
		case PAD: return "P";
		case ROCK: return "R";
		case ROCKONPAD:	return "\u1E56"; //P with a dot above
		case ROCKONTELEPORTAL: return "\u1E6A"; //T with a dot above
		case TELEPORTAL: return "T";
		}
		return null;	
	}
}
