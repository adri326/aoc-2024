with Ada.Containers.Vectors;
with Integer_Vectors; use Integer_Vectors;


package Grid is
   package Integer_Grids is new Ada.Containers.Vectors(
      Index_Type => Natural,
      Element_Type => Integer_Vectors.Vector
   );

   package Integer_Semigrids is new Ada.Containers.Vectors(
      Index_Type => Natural,
      Element_Type => Integer_Vectors.Vector
   );

   subtype Integer_Grid is Integer_Grids.Vector;
   subtype Integer_Semigrid is Integer_Grids.Vector;

   Dimension_Except : exception;

   function Load (Path: String) return Integer_Semigrid;

   function To_Grid (Semi : Integer_Semigrid) return Integer_Grid;
   function To_Semigrid (G : Integer_Grid) return Integer_Semigrid;

   procedure Print (G: Integer_Semigrid);
end Grid;
