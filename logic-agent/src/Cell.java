
public class Cell {
	
	private CellType type;
	

	public Cell(CellType type) {
		this.setType(type) ;
	}

	public CellType getType() {
		return type;
	}

	public void setType(CellType type) {
		this.type = type;
	}
	public Cell clone(){
		return new Cell(this.type);
	}
	public String toString(){
		/*
		 * Returns the cell type.
		 */
		return type.toString();
	}
}
