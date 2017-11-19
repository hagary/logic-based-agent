import java.io.FileNotFoundException;

public class Main {
	final static int gridLimit = 5; //upper bound for dimensions of the grid.

	public static void main (String[]args) throws Exception{
		Grid grid1 = Grid.createGridFromFile("Sol1");
		Grid grid2 = Grid.createGridFromFile("Sol2");
		Grid grid3 = Grid.createGridFromFile("Sol3");
		
		GenGrid(grid1);
		GenGrid(grid2);
		GenGrid(grid3);
		
		System.out.println(grid1);
		System.out.println(grid2);
		System.out.println(grid3);
	}
	public static void GenGrid(Grid g) throws FileNotFoundException{
		g.writePrologFacts();
		g.writePrologQuery();
	}
}