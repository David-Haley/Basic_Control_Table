-- Package to provided type declarations for Basic_CT.

-- Author    : David Haley
-- Created   : 24/03/2023
-- Last Edit : 09/04/2023
-- 20230409 : for adjacent track linkage Track_Name and Track_End consolidated
-- into Track_Keys.
-- 20230408 : Track_Names  and Route_Names made subtypes.
-- 20230402 : In signals Replacement_Track type becomes Track_Keys.
-- Subtype Main_Route_Classes added.
-- 20230328 : Correction of spelling to Entrance_End.
-- 20230327 : Additional default values added.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Maps;

package CT_Types is

   type Track_Types is (Plain, Points, Diamond, Switch_Diamond);

   subtype Track_Names is Unbounded_String;
   subtype Track_Ends is Character with
     Static_Predicate => Track_Ends in 'a' .. 'z';
   -- N.B limits tracks to having no more than 26 ends this implies that a
   -- single logical track can contain no more than 8 sets of points.

   type Track_Keys is record
      Track_Name : Track_Names;
      Track_End : Track_Ends;
   end record; -- Track_Keys;

   function "<" (Left, Right : Track_Keys) return Boolean;

   Null_Link : constant Track_Keys := (Null_Unbounded_String, 'z');

   type Metres is new Natural;
   Minimum_Track_Length : constant Metres := 15;

   type Point_Numbers is new Positive;
   subtype Point_Ends is Character with
     Static_Predicate => Point_Ends in 'A' .. 'Z';

   type End_Elements is record
      This_End : Track_Ends := 'z';
      Adjacent : Track_Keys := Null_Link;
      Length : Metres := 0; -- Measured from TOS or centre of diamond.
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
         Left_End, Right_End : Track_Ends := 'z';
         Adjacent_Left, Adjacent_Right : Track_Keys := Null_Link;
         Length : Metres := 0;
      when Points =>
         Points_Number : Point_Numbers;
         Is_Single_Ended : Boolean := True;
         Points_End : Point_Ends := 'A';
         Has_Swing_Nose : Boolean := False;
         Swing_Nose_End : Point_Ends := 'Z';
         Normal_Is_Straight : Boolean := True;
         Points_LHSNC : Boolean;
         Point_End_Array : Point_End_Arrays;
      when Diamond =>
         Diamond_End_Array : Diamond_End_Arrays;
      when Switch_Diamond =>
         Diamond_Number : Point_Numbers;
         Diamond_End : Point_Ends := 'A';
         Has_Left_Swing_Nose, Has_Right_Swing_Nose : Boolean := False;
         Left_Swing_Nose_End : Point_Ends := 'Z';
         Right_Swing_Nose_End : Point_Ends := 'Z';
         Diamond_LHSNC : Boolean;
         -- it is assuned that whatever is considered straight is Normal.
         -- Straight should be defined to be the Normal lie.
         Switch_Diamond_End_Array : Diamond_End_Arrays;
         -- N.B. must have different name but has the same declaration as fixed
         -- Diamond
      end case; -- Track_Type
   end record; -- Tracks

   subtype Track_Indices is Positive;

   package Track_Stores is new
     Ada.Containers.Indefinite_Vectors (Track_Indices, Tracks);

   package Track_Dictionaries is new
     Ada.Containers.Ordered_Maps (Track_Keys, Track_Indices);

   type Signal_Numbers is new Positive;

   type Signals is record
      Is_Main, Is_Shunt : Boolean;
      Replacement_Track : Track_Keys;
   end record; -- Signals

   package Signal_Stores is new
     Ada.Containers.Ordered_Maps (Signal_Numbers, Signals);

   package Sub_Route_to_Signal_Maps is new
     Ada.Containers.Ordered_Maps (Track_Keys, Signal_Numbers);

   subtype Route_Names is Unbounded_String;
   type Route_Classes is (Main, Call_On, Warner, Shunt);
   subtype Main_Route_Classes is Route_Classes range Main .. Call_on;

   type Routes is record
      Entrance_Signal, Exit_Signal : Signal_Numbers;
      Route_Class : Route_Classes;
   end record; -- Routes;

   package Route_Stores is new
     Ada.Containers.Ordered_Maps (Route_Names, Routes);

   type Sub_Routes is record
      Track_Name : Track_Names;
      Entrance_End, Exit_End : Track_Ends;
   end record; -- Sub_Routes

   subtype Sub_Route_Indices is Positive;

   package Sub_Route_Lists is new
     Ada.Containers.Vectors (Sub_Route_Indices, Sub_Routes);

   function "=" (Left, Right : Sub_Route_Lists.Vector) return Boolean;
   -- Required for Route_Maps declaration

   package Route_Maps is new
     Ada.Containers.Ordered_Maps (Route_Names, Sub_Route_Lists.Vector);

end CT_Types;
