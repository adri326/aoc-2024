with Ada.Containers.Vectors;

package Split_Line is
    package Integer_Vectors is new Ada.Containers.Vectors(
        Index_Type => Natural,
        Element_Type => Integer
    );

    function Split (Line : String) return Integer_Vectors.Vector;
end Split_Line;
