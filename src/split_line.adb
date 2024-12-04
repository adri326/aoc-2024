with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers.Vectors;

package body Split_Line is
    function Split (Line : String) return Integer_Vectors.Vector is
        Current_Token : Unbounded_String;
        Digit_Set : Ada.Strings.Maps.Character_Set :=
            Ada.Strings.Maps.To_Set(Span => (Low => '0', High => '9'));

        First : Natural;
        Last : Natural := 0;
        Result : Integer_Vectors.Vector;
    begin
        loop
            Ada.Strings.Fixed.Find_Token(
                Source => Line,
                Set => Digit_Set,
                Test => Ada.Strings.Inside,
                From => Last + 1,
                First => First,
                Last => Last
            );
            exit when Last = 0;
            Result.append(Integer'Value(Line(First .. Last)));

            exit when First >= Line'Length;
        end loop;

        return Result;
    end Split;
end Split_Line;
