-- Package to provided type declarations for Basic_CT.

-- Author    : David Haley
-- Created   : 24/03/2023
-- Last Edit : 25/03/2023

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

package CT_Types is

   type Track_Types is (Plain, Points, Diamond, Switch_Diamond);

   type Track_Names is new Unbounded_String;
   subtype Track_Ends is Character with
     Static_Predicate => Track_Ends in '0' .. '1' | 'a' .. 'z';

   type Metres is new Natural;
   Minimum_Track_Length : constant Metres := 15;

   type Point_Numbers is new Positive;
   subtype Point_Ends is Character with
     Static_Predicate => Point_Ends in 'A' .. 'Z';

   type End_Elements is record
      Adjacent_Track : Track_Names;
      Adjacent_End : Track_Ends;
      Length : Metres; -- Measured from TOS oe centre of diamond.
      Is_Clear : Boolean := True;
   end record; -- End_Elements

   type Point_End_Indices is (Facing, Straight, Divergent);

   type Point_End_Arrays is array (Point_End_Indices) of End_Elements;

   type Diamond_End_Indices is (Left_Straight, Right_Straight, Left_Cross,
                                Right_Cross);

   type Diamond_End_Arrays is array (Diamond_End_Indices) of End_Elements;

   type Tracks (Track_Type : Track_Types) is record
      Track_Name : Track_Names;
      case Track_Type is
      when Plain =>
         Adjacent_Left_Track, Adjacent_Right_Track : Track_Names;
         Left_End, Adjacent_Right_End : Track_Ends := 'a';
         Right_End, Adjacent_Left_End : Track_Ends := 'b';
         Length : Metres;
      when Points =>
         Points_Number : Point_Numbers;
         Is_Single_Ended : Boolean := True;
         Points_End : Point_Ends := 'A';
         Has_Swing_Nose : Boolean := False;
         Swing_Nose_End : Point_Ends := 'B';
         Normal_Is_Straight : Boolean := True;
         Points_LHSNC : Boolean;
         Point_End_Array : Point_End_Arrays;
      when Diamond =>
         Diamond_End_Array : Diamond_End_Arrays;
      when Switch_Diamond =>
         Diamond_Number : Point_Numbers;
         Diamond_End : Point_Ends := 'D';
         Has_Left_Swing_Nose, Has_Right_Swing_Nose : Boolean := True;
         Left_Swing_Nose_End : Point_Ends := 'C';
         Right_Swing_Nose_End : Point_Ends := 'E';
         Diamond_LHSNC : Boolean;
         -- it is assuned that whatever is considered straight is Normal.
         -- Straight should be defined to be made in the Normal lie
         Switched_Diamond_End_Array : Diamond_End_Arrays;
         -- N.B. must have different name but has the same declaration as fixed
         -- Diamond
      end case; -- Track_Type
   end record; -- Tracks

   subtype Track_Indices is Positive;

   package Track_Stores is new
     Ada.Containers.Indefinite_Vectors (Track_Indices, Tracks);

   type Signal_Numbers is new Positive;

   type Signals is record
      Signal_Number : Signal_Numbers;
      Is_Main, Is_Shunt : Boolean;
      Replacement_Track : Track_Names;
      Entrence_End : Track_Ends;
   end record; -- Signals

   subtype Signal_Indices is Positive;

   package Signal_Stores is new
     Ada.Containers.Vectors (Signal_Indices, Signals);

   type Route_Names is new Unbounded_String;
   type Route_Classes is (Main, Call_On, Shunt, Warner);

   type Routes is record
      Route_Name : Route_Names;
      Entrance_Signal, Exit_Signal : Signal_Numbers;
      Route_Class : Route_Classes;
   end record; -- Routes;

   subtype Route_Indices is Positive;

   package Route_Stores is new Ada.Containers.Vectors (Route_Indices, Routes);

end CT_Types;
