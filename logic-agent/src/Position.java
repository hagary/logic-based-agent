enum DistanceType{
	CITYBLOCK, EUCLID
}
public class Position {
	private int x;
	private int y;

	public Position(int x, int y) {
		this.x = x;
		this.y = y;
	}

	public int getX() {
		return x;
	}

	public void setX(int x) {
		this.x = x;
	}

	public int getY() {
		return y;
	}

	public void setY(int y) {
		this.y = y;
	}
	public void set(Position p){
		this.x = p.x;
		this.y = p.y;
	}
	public boolean equals(int x, int y){
		return (this.x==x) && (this.y==y);
	}
	public boolean equals(Position p){
		return (this.x==p.x) && (this.y==p.y);
	}
	public Position clone(){
		return new Position(x, y);
	}
	private int cityBlockDistanceTo(Position p){
		/*
		 * Returns the difference in x + difference in y.
		 */
		return Math.abs(p.x - this.x) + Math.abs(p.y - this.y);
	}
	private int euclidDistanceTo(Position p){
		int deltaX = p.x - this.x;
		int deltaY = p.y - this.y;
		return (int) Math.sqrt(deltaX*deltaX + deltaY*deltaY);
	}
	public int distanceTo(Position p, DistanceType distType){
		switch(distType){
		case CITYBLOCK: return cityBlockDistanceTo(p);
		case EUCLID: return euclidDistanceTo(p);
		default: return 0;
		}
	}
	public String toString(){
		return "("+ x + "," + y + ")";
	}
}
