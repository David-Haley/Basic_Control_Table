-- Program to create a basic control table from track layout information input
-- as CSV tables. This is likely to be subject to scope creep but initially
-- in-route signal to signal and signal to points locking is targeted as being
-- Most critical to safety and least variable in terms of signalling rules.

-- Author    : David Haley
-- Created   : 24/03/2023
-- Last Edit : 23/04/2023
-- 20230423 : Signal Numbers made a string to allow for a prefix nmenonic.
-- Track_Stores and Sub_Route_Lists changed from vector to doubly linked list.
-- Points route holding tracks extended to include next inroute track if
-- Straight or Divergent ends are not clear, similarly for Switch_Diamond.
-- 20230412 : Points Route Holding added to listing.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with CT_Types; use CT_Types;
with Get_Data; use Get_Data;
with Build_Structures; use Build_Structures;
with DJH.Date_and_Time_Strings; use DJH.Date_and_Time_Strings;
use CT_Types.Route_Maps;
use CT_Types.Sub_Route_Lists;

procedure Basic_Ct is

   Solid_Line : constant String := 80 * '_';
   Dotted_Line : constant String := 80 * '.';

   use Route_Stores;

   function Output_Path return String is

      -- Returns the to the path where the output files are to be sent. If the
      -- path does not exist it will attempt to creat it, thie could fail,
      -- causing an exception to be raised.

   begin -- Output_Path
      if Argument_Count = 1 then
         declare -- Path declaration block
            Path : String := Argument (1);
         begin
            if Exists (Path) then
               if Kind (Path) = Directory then
                  return Path;
               else
                  raise Ada.Text_IO.Use_Error with "Path """ & Argument (1) &
                    """ is not a directory";
               end if; -- Kind (Path) = Directory
            else
               Create_Directory (Path);
               return Path;
            end if; -- Exists (Path)
         end; -- Path declaration block
      else
         return "";
      end if; -- Argument_Count = 1
   end Output_Path;

   procedure Header (Output_File : in out File_Type;
                     Route : in Route_Names;
                     Route_Store : in Route_Stores.Map) is

   begin -- Header
      Put_Line ("Writing CT for " & Route);
      Put_Line (Output_File, "Date: " & Date_String & "  Time: " & Time_String);
      Put_Line (Output_File, "Route: " & Route & " from " &
                  Route_Store (Route).Entrance_Signal & " to " &
                  Route_Store (Route).Exit_Signal & " Route class " &
                  Route_Store (Route).Route_Class'Img);
      Put_Line (Output_File, Solid_Line);
   end Header;

   procedure Tracks_Clear (Output_File : in out File_Type;
                           Route : in Route_Names;
                           Route_Store : In Route_Stores.Map;
                           Track_List : in Track_Lists.List) is

      -- Reports inroute tracks required clear.

      use track_lists;

   begin -- Tracks_Clear
      Put_Line (Output_File, "Aspect requires:-");
      Put (Output_File, "Tracks Clear:");
      if Route_Store (Route).Route_Class /= Shunt then
         for T in Iterate (Track_List) loop
            if T /= Last (Track_List) or else
              Route_Store (Route).Route_Class /= Call_On then
               Put (Output_File, " " & Track_List (T));
            end if; -- T /= Last (Track_List)
         end loop; -- T in Iterate (Track_List)
      end if; -- Route_Store (Route).Class /= Shunt
      New_Line (Output_File);
      Put_Line (Output_File, Dotted_Line);
   end Tracks_Clear;

   procedure Points_Detection (Output_File : in out File_Type;
                               Point_List : in Point_Lists.List) is

      use Point_Lists;
      use Point_End_Sets;

   begin -- Points_Detection
      for Lie in Point_Lies range N .. R loop
         Put (Output_File, "Points " & Lie'Img & ':');
         for P in Iterate (Point_List) loop
            if Lie = Point_List (P).Required_Lie then
               If Length (Point_List (P).In_Route_Ends) = 1 then
                  Put (Output_File, Point_List (P).Point_Number'img);
               else
                  for E in Iterate (Point_List (P).In_Route_Ends) loop
                     Put (Output_File, Point_List (P).Point_Number'img &
                            Element (E));
                  end loop; -- E in Iterate (Point_List (P).In_Route_Ends)
               end if; -- Length (Point_List (P).In_Route_Ends) = 1
            elsif Point_List (P).Required_Lie = Undefined then
               raise Program_Error with "Required lie of" &
                 Point_List (P).Point_Number'img & " not resolved";
            end if; -- Lie = Point_List (P).Required_Lie
         end loop; -- P in Iterate (Point_List)
         New_Line (Output_File);
         if Lie = N then
            Put_Line (Output_File, Dotted_Line);
         else
            Put_Line (Output_File, Solid_Line);
         end if; -- Lie = N
      end loop; -- Lie in Point_Lies range N .. R
   end Points_Detection;

   procedure Route_Holding_Points (Output_File : in out File_Type;
                                   Track_List : in Track_Lists.List;
                                   Point_List : in Point_Lists.List) is

      use Track_Lists;
      use Point_Lists;

      Tc : Track_Lists.Cursor;

   begin -- Route_Holding_Points
      Put_Line (Output_File, "Points route held by tracks occupied:-");
      for P in Iterate (Point_List) loop
         Put (Output_File, Point_List (P).Point_Number'img & " " &
                Point_List (P).Required_Lie'Img & " :");
         Tc := First (Track_List);
         loop -- until last holding track
            Put (Output_File, " " & Track_List (Tc));
            exit when Track_List (Tc) = Point_List (P).Last_Holding_Track;
            Next (Tc);
         end loop; -- until last holding track
         New_Line (Output_File);
      end loop; -- P in Iterate (Point_List)
      Put_Line (Output_File, Solid_Line);
   end Route_Holding_Points;

   Track_Store : Track_Stores.List;
   Signal_Store : Signal_Stores.Map;
   Route_Store : Route_Stores.Map;
   Track_Dictionary : Track_Dictionaries.Map;
   Sub_Route_to_Signal_Map : Sub_Route_to_Signal_Maps.Map;
   Route_Map : Route_Maps.Map;
   Output_File : File_Type;
   Track_List : Track_Lists.List;
   Point_List : Point_Lists.List;

begin
   Put_Line ("Basic Control Table 20230423");
   Get (Track_Store);
   Get (Signal_Store);
   Get (Route_Store);
   Build (Track_Store, Track_Dictionary);
   Build (Signal_Store, Track_Dictionary, Sub_Route_to_Signal_Map);
   Build (Track_Store, Signal_Store, Route_Store, Track_Dictionary,
          Sub_Route_to_Signal_Map, Route_Map);
   for R in Iterate (Route_Store) loop
      Create (Output_File, Out_File, Compose (Output_Path, To_String (Key (R)),
              "txt"));
      Header (Output_File, Key (R), Route_Store);
      Build (Route_Map (Key (R)), Track_List);
      Tracks_Clear (Output_File, Key (R), Route_Store, Track_List);
      Build (Track_Store, Track_Dictionary, Route_Map (Key (R)), Point_List);
      Points_Detection (Output_File, Point_List);
      Route_Holding_Points (Output_File, Track_List, Point_List);
      Put_Line (Output_File, "End of text for " & Key (R));
      Close (Output_File);
   end loop; -- R in Iterate (Route_Store)
   Put_Line ("Processing Complete");
end Basic_Ct;
