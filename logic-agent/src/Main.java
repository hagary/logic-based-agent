public class Main {
	final static int gridLimit = 5; //upper bound for dimensions of the grid.

	public static void main (String[]args) throws Exception{
		/*
		 * Uncomment one of the following lines to create a grid from a given file.
		 */
		Grid grid1 = Grid.createGridFromFile("Sol1");
		Grid grid2 = Grid.createGridFromFile("Sol2");
		Grid grid3 = Grid.createGridFromFile("Sol3");
		grid1.writePrologFacts();
		grid1.writePrologQuery();
		grid2.writePrologFacts();
		grid2.writePrologQuery();
		grid3.writePrologFacts();
		grid3.writePrologQuery();
		System.out.println(grid1);
		System.out.println(grid2);
		System.out.println(grid3);
	}
}