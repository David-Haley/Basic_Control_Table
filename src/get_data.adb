-- Package to read input data for Basic_CT.

-- Author    : David Haley
-- Created   : 25/03/2023
-- Last Edit : 25/03/2023

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
-- with Ada.Containers.Vectors;
with DJH.Parse_CSV;
with CT_Types; use CT_Types;

package body Get_Data is

   CSV : constant String := "csv";

   procedure Plain_Track (Track_Store : in out Track_Stores.Vector) is

      Track_File_Name : constant String := "Tracks";

      type Track_Header is
        (Track_Name, Left_End, Right_End,
         Adjacent_Left_Track, Adjacent_Left_End,
         Adjacent_Right_Track, Adjacent_Right_End, Length);

      package Tracks_CSV is new DJH.Parse_CSV (Track_Header);
      use Tracks_CSV;

      Track : Tracks (Plain);

   begin -- Plain_Track
      Read_Header (Compose (Name => Track_File_Name, Extension => CSV));
      Put_Line ("Reading " &
                  Compose (Name => Track_File_Name, Extension => CSV));
      while Next_Row loop
         begin -- record exception block
            Track.Track_Name := To_Unbounded_String (Get_Value (Track_Name));
            if Get_Value (Left_End)'Length = 1 then
               Track.Left_End :=  Get_Value (Left_End) (1);
            else
               raise Data_Error with "Invalid data for Left_End";
            end if; -- Get_Value (Left_End)'Length = 1
            if Get_Value (Right_End)'Length = 1 then
               Track.Right_End :=  Get_Value (Right_End) (1);
            else
               raise Data_Error with "Invalid data for Right_End";
            end if; -- Get_Value (Right_End)'Length = 1
            Track.Adjacent_Left_Track :=
              To_Unbounded_String (Get_Value (Adjacent_Left_Track));
            if Get_Value (Adjacent_Left_End)'Length = 1 then
               Track.Adjacent_Left_End :=  Get_Value (Adjacent_Left_End) (1);
            else
               raise Data_Error with "Invalid data for Adjacent_Left_End";
            end if; -- Get_Value (Adjacent_Left_End)'Length = 1
            Track.Adjacent_Right_Track :=
              To_Unbounded_String (Get_Value (Adjacent_Right_Track));
            if Get_Value (Adjacent_Right_End)'Length = 1 then
               Track.Adjacent_Right_End :=  Get_Value (Adjacent_Right_End) (1);
            else
               raise Data_Error with "Invalid data for Adjacent_Right_End";
            end if; -- Get_Value (Adjacent_Right_End)'Length = 1
            Track.Length := Metres'Value (Get_Value (Length));
            Track_Stores.Append (Track_Store, Track);
         exception
            when E: others =>
               Put_Line ("Row:" & Row_Number'Image & " > " & Exception_Name (E)
                         & " - " & Exception_Message (E));
         end; -- record exception block
      end loop; -- Next_Row
      Close_CSV;
   end Plain_Track;

   procedure Points_Track (Track_Store : in out Track_Stores.Vector) is

      Track_File_Name : constant String := "Points";

      type Points_Header is
        (Track_Name, Points_Number, Is_Single_Ended, Points_End,
         Has_Swing_Nose, Swing_Nose_End, Normal_Is_Straight, Points_LHSNC,
         Adjacent_Track_Facing, Adjacent_End_Facing, Length_Facing,
         Is_Clear_Facing, Adjacent_Track_Straight,  Adjacent_End_Straight,
         Length_Straight, Is_Clear_Straight, Adjacent_Track_Divergent,
         Adjacent_End_Divergent, Length_Divergent, Is_Clear_Divergent);

      package Tracks_CSV is new DJH.Parse_CSV (Points_Header);
      use Tracks_CSV;

      Track : Tracks (Points);

   begin -- Points_Track
      Read_Header (Compose (Name => Track_File_Name, Extension => CSV));
      Put_Line ("Reading " &
                  Compose (Name => Track_File_Name, Extension => CSV));
      while Next_Row loop
         begin -- record exception block
            Track.Track_Name := To_Unbounded_String (Get_Value (Track_Name));
            Track.Points_Number :=
              Point_Numbers'Value (Get_Value (Points_Number));
            Track.Is_Single_Ended :=
              Boolean'Value (Get_Value (Is_Single_Ended));
            if Get_Value (Points_End)'Length = 1 then
               Track.Points_End := Get_Value (Points_End) (1);
            else
               raise Data_Error with "Invalid data for Points_End";
            end if; -- Get_Value (Points_End)'Length = 1
            Track.Has_Swing_Nose := Boolean'Value (Get_Value (Has_Swing_Nose));
            if Get_Value (Swing_Nose_End)'Length = 1 then
               Track.Swing_Nose_End := Get_Value (Swing_Nose_End) (1);
            else
               raise Data_Error with "Invalid data for Swing_Nose_End";
            end if; -- Get_Value (Swing_Nose_End)'Length = 1
            Track.Normal_Is_Straight :=
              Boolean'Value (Get_Value (Normal_Is_Straight));
            Track.Points_LHSNC := Boolean'Value (Get_Value (Points_LHSNC));
            -- Facing end
            Track.Point_End_Array (Facing).Adjacent_Track :=
              To_Unbounded_String (Get_Value (Adjacent_Track_Facing));
            if Get_Value (Adjacent_End_Facing)'Length = 1 then
               Track.Point_End_Array (Facing).Adjacent_End :=
                 Get_Value (Adjacent_End_Facing) (1);
            else
               raise Data_Error with "Invalid data for Adjacent_End_Facing";
            end if; -- Get_Value (Adjacent_End_Facing)'Length
            Track.Point_End_Array (Facing).Length :=
              Metres'Value (Get_Value (Length_Facing));
            Track.Point_End_Array (Facing).Is_Clear :=
              Boolean'Value (Get_Value (Is_Clear_Facing));
            -- Straight end
            Track.Point_End_Array (Straight).Adjacent_Track :=
              To_Unbounded_String (Get_Value (Adjacent_Track_Straight));
            if Get_Value (Adjacent_End_Straight)'Length = 1 then
               Track.Point_End_Array (Straight).Adjacent_End :=
                 Get_Value (Adjacent_End_Straight) (1);
            else
               raise Data_Error with "Invalid data for Adjacent_End_Straight";
            end if; -- Get_Value (Adjacent_End_Straight)'Length
            Track.Point_End_Array (Straight).Length :=
              Metres'Value (Get_Value (Length_Straight));
            Track.Point_End_Array (Straight).Is_Clear :=
              Boolean'Value (Get_Value (Is_Clear_Straight));
            -- Divergent end
            Track.Point_End_Array (Divergent).Adjacent_Track :=
              To_Unbounded_String (Get_Value (Adjacent_Track_Divergent));
            if Get_Value (Adjacent_End_Divergent)'Length = 1 then
               Track.Point_End_Array (Divergent).Adjacent_End :=
                 Get_Value (Adjacent_End_Divergent) (1);
            else
               raise Data_Error with "Invalid data for Adjacent_End_Divergent";
            end if; -- Get_Value (Adjacent_End_Divergent)'Length
            Track.Point_End_Array (Divergent).Length :=
              Metres'Value (Get_Value (Length_Divergent));
            Track.Point_End_Array (Divergent).Is_Clear :=
              Boolean'Value (Get_Value (Is_Clear_Divergent));
            Track_Stores.Append (Track_Store, Track);
         exception
            when E: others =>
               Put_Line ("Row:" & Row_Number'Image & " > " & Exception_Name (E)
                         & " - " & Exception_Message (E));
         end; -- record exception block
      end loop; -- Next_Row
      Close_CSV;
   end Points_Track;

   procedure Get (Track_Store : out Track_Stores.Vector) is

      type Diamond_Header is
        (Track_Name, Adjacent_Track_Left_Straight, Adjacent_End_Left_Straight,
         Length_Left_Straight, Is_Clear_Left_Straight,
         Adjacent_Track_Right_Straight, Adjacent_End_Right_Straight,
         Length_Right_Straight, Is_Clear_Right_Straight,
         Adjacent_Track_Left_Cross, Adjacent_End_Left_Cross, Length_Left_Cross,
         Is_Clear_Left_Cross, Adjacent_Track_Right_Cross,
         Adjacent_End_Right_Cross, Length_Right_Cross, Is_Clear_Right_Cross);

      type Switch_Diamond_Header is
        (Track_Name, Diamond_Number, Diamond_End, Has_Left_Swing_Nose,
         Has_Right_Swing_Nose, Left_Swing_Nose_End, Right_Swing_Nose_End,
         Diamond_LHSNC, Adjacent_Track_Left_Straight,
         Adjacent_End_Left_Straight, Length_Left_Straight,
         Is_Clear_Left_Straight, Adjacent_Track_Right_Straight,
         Adjacent_End_Right_Straight, Length_Right_Straight,
         Is_Clear_Right_Straight, Adjacent_Track_Left_Cross,
         Adjacent_End_Left_Cross, Length_Left_Cross, Is_Clear_Left_Cross,
         Adjacent_Track_Right_Cross, Adjacent_End_Right_Cross,
         Length_Right_Cross, Is_Clear_Right_Cross);

   begin -- Get
      Track_Stores.Clear (Track_Store);
      Plain_Track (Track_Store);
      Points_Track (Track_Store);
   end Get;

   procedure Get (Signal_Store : out Signal_Stores.Vector) is

      Signal_File_Name : constant String := "Signals";

      type Signal_Header is
        (Signal_Number, Is_Main, Is_Shunt, Replacement_Track, Entrence_End);

      package Signals_CSV is new DJH.Parse_CSV (Signal_Header);
      use Signals_CSV;

      Signal : Signals;

   begin -- Get
      Signal_Stores.Clear (Signal_Store);
      Read_Header (Compose (Name => Signal_File_Name, Extension => CSV));
      Put_Line ("Reading " &
                  Compose (Name => Signal_File_Name, Extension => CSV));
      while Next_Row loop
         begin -- record exception block
            Signal.Signal_Number :=
              Signal_Numbers'Value (Get_Value (Signal_Number));
            Signal.Is_Main := Boolean'Value (Get_Value (Is_Main));
            Signal.Is_Shunt := Boolean'Value (Get_Value (Is_Shunt));
            Signal.Replacement_Track :=
              To_Unbounded_String (Get_Value (Replacement_Track));
            if Get_Value (Entrence_End)'Length = 1 then
               Signal.Entrence_End := Get_Value (Entrence_End) (1);
            else
               raise Data_Error with "invalid data for Entrence_End";
            end if; --  Get_Value (Entrence_End)'Length = 1
            Signal_Stores.Append (Signal_Store, Signal);
         exception
            when E: others =>
               Put_Line ("Row:" & Row_Number'Image & " > " & Exception_Name (E)
                         & " - " & Exception_Message (E));
         end; -- record exception block
      end loop; -- Next_Row
      Close_CSV;
   end Get;

   procedure Get (Route_Store : out Route_Stores.Vector) is

      Route_File_Name : constant String := "Routes";

      Route : Routes;

      type Route_Header is
        (Route_Name, Entrance_Signal, Exit_Signal, Route_Class);

      package Route_CSV is new DJH.Parse_CSV (Route_Header);
      use Route_CSV;

   begin -- Get
      Route_Stores.Clear (Route_Store);
      Read_Header (Compose (Name => Route_File_Name, Extension => CSV));
      Put_Line ("Reading " &
                  Compose (Name => Route_File_Name, Extension => CSV));
      while Next_Row loop
         begin -- record exception block
            Route.Route_Name := To_Unbounded_String (Get_Value (Route_Name));
            Route.Entrance_Signal :=
              Signal_Numbers'Value (Get_Value (Entrance_Signal));
            Route.Exit_Signal := Signal_Numbers'Value (Get_Value (Exit_Signal));
            Route.Route_Class := Route_Classes'Value (Get_Value (Route_Class));
            Route_Stores.Append (Route_Store, Route);
         exception
            when E: others =>
               Put_Line ("Row:" & Row_Number'Image & " > " & Exception_Name (E)
                         & " - " & Exception_Message (E));
         end; -- record exception block
      end loop; -- Next_Row
      Close_CSV;
   end Get;

end Get_Data;
