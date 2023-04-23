-- Package to read input data for Basic_CT.

-- Author    : David Haley
-- Created   : 25/03/2023
-- Last Edit : 23/04/2023
-- 20230423 : Signal Numbers made a string to allow for a prefix nmenonic.
-- Track_Stores and Sub_Route_Lists changed from vector to doubly linked list.
-- Check that Signal_Numbers and Routes are unique;
-- 20230410 : Corrected reading of points swing node data.
-- 20230409 : for adjacent track linkage Track_Name and Track_End consolidated
-- into Track_Keys, sall to Append was missing from Diamond and Switch_Diamond.
-- 20230408 : Correction of reading single characters.
-- 20230402 : In signals Replacement_Track type becomes Track_Keys.
-- 20230328 : Correction of spelling to Entrance_End.
-- 20230327 : Reading of adjacent end made conditional on there being an
-- adjacent track name defined. Reading of swing noses end identifiers made
-- conditional on swing nose being defined as True.

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DJH.Parse_CSV;
with CT_Types; use CT_Types;
with File_Heders; use File_Heders;

package body Get_Data is

   function First_Character (Str : in String) return Character is
      (Str (Str'First));

   procedure Plain_Track (Track_Store : in out Track_Stores.List) is

      package Tracks_CSV is new DJH.Parse_CSV (Track_Header);
      use Tracks_CSV;

   begin -- Plain_Track
      Read_Header (Compose (Name => Track_File_Name, Extension => CSV));
      Put_Line ("Reading " &
                  Compose (Name => Track_File_Name, Extension => CSV));
      while Next_Row loop
         declare
            Track : Tracks (Plain);
         begin -- record exception block
            Track.Track_Name := To_Unbounded_String (Get_Value (Track_Name));
            if Get_Value (Left_End)'Length = 1 then
               Track.Left_End := First_Character (Get_Value (Left_End));
            else
               raise Data_Error with "Invalid data for Left_End";
            end if; -- Get_Value (Left_End)'Length = 1
            if Get_Value (Right_End)'Length = 1 then
               Track.Right_End := First_Character (Get_Value (Right_End));
            else
               raise Data_Error with "Invalid data for Right_End";
            end if; -- Get_Value (Right_End)'Length = 1
            Track.Adjacent_Left.Track_Name :=
              To_Unbounded_String (Get_Value (Adjacent_Left_Track));
            if Length (Track.Adjacent_Left.Track_Name) > 0 then
               if Get_Value (Adjacent_Left_End)'Length = 1 then
                  Track.Adjacent_Left.Track_End :=
                    First_Character (Get_Value (Adjacent_Left_End));
               else
                  raise Data_Error with "Invalid data for Adjacent_Left_End";
               end if; -- Get_Value (Adjacent_Left_End)'Length = 1
            end if; -- Length (Track.Adjacent_Left.Track_Name) > 0
            Track.Adjacent_Right.Track_Name :=
              To_Unbounded_String (Get_Value (Adjacent_Right_Track));
            if Length (Track.Adjacent_Right.Track_Name) > 0 then
               if Get_Value (Adjacent_Right_End)'Length = 1 then
                  Track.Adjacent_Right.Track_End :=
                    First_Character (Get_Value (Adjacent_Right_End));
               else
                  raise Data_Error with "Invalid data for Adjacent_Right_End";
               end if; -- Get_Value (Adjacent_Right_End)'Length = 1
            end if; -- Length (Track.Adjacent_Right.Track_Name)
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

   procedure Points_Track (Track_Store : in out Track_Stores.List) is

      package Tracks_CSV is new DJH.Parse_CSV (Points_Header);
      use Tracks_CSV;

   begin -- Points_Track
      Read_Header (Compose (Name => Points_File_Name, Extension => CSV));
      Put_Line ("Reading " &
                  Compose (Name => Points_File_Name, Extension => CSV));
      while Next_Row loop
         declare
            Track : Tracks (Points);
         begin -- record exception block
            Track.Track_Name := To_Unbounded_String (Get_Value (Track_Name));
            Track.Points_Number :=
              Point_Numbers'Value (Get_Value (Points_Number));
            Track.Is_Single_Ended :=
              Boolean'Value (Get_Value (Is_Single_Ended));
            if not Track.Is_Single_Ended then
               if Get_Value (Points_End)'Length = 1 then
                  Track.Points_End := First_Character (Get_Value (Points_End));
               else
                  raise Data_Error with "Invalid data for Points_End";
               end if; -- Get_Value (Points_End)'Length = 1
               Track.Has_Swing_Nose :=
                 Boolean'Value (Get_Value(Has_Swing_Nose));
               if Track.Has_Swing_Nose then
                  if Get_Value (Swing_Nose_End)'Length = 1 then
                     Track.Swing_Nose_End :=
                       First_Character (Get_Value (Swing_Nose_End));
                  else
                     raise Data_Error with "Invalid data for Swing_Nose_End";
                  end if; -- Get_Value (Swing_Nose_End)'Length = 1
               end if; -- Track.Has_Swing_Nose
            end if; -- not Track.Is_Single_Ended
            Track.Normal_Is_Straight :=
              Boolean'Value (Get_Value (Normal_Is_Straight));
            Track.Points_LHSNC := Boolean'Value (Get_Value (Points_LHSNC));
            -- Facing end
            if Get_Value (This_End_Facing)'Length = 1 then
               Track.Point_End_Array (Facing).This_End :=
                 First_Character (Get_Value (This_End_Facing));
            else
               raise Data_Error with "Invalid data for This_End_Facing";
            end if; -- Get_Value (This_End_Facing)'Length = 1
            Track.Point_End_Array (Facing).Adjacent.Track_Name :=
              To_Unbounded_String (Get_Value (Adjacent_Track_Facing));
            if Length (Track.Point_End_Array (Facing).Adjacent.
                         Track_Name) > 0 then
               if Get_Value (Adjacent_End_Facing)'Length = 1 then
                  Track.Point_End_Array (Facing).Adjacent.Track_End :=
                    First_Character (Get_Value (Adjacent_End_Facing));
               else
                  raise Data_Error with "Invalid data for Adjacent_End_Facing";
               end if; -- Get_Value (Adjacent_End_Facing)'Length
            end if; -- Length (Track.Point_End_Array (Facing).Adjacent ...
            Track.Point_End_Array (Facing).Length :=
              Metres'Value (Get_Value (Length_Facing));
            Track.Point_End_Array (Facing).Is_Clear :=
              Boolean'Value (Get_Value (Is_Clear_Facing));
            -- Straight end
            if Get_Value (This_End_Straight)'Length = 1 then
               Track.Point_End_Array (Straight).This_End :=
                 First_Character (Get_Value (This_End_Straight));
            else
               raise Data_Error with "Invalid data for This_End_Straight";
            end if; -- Get_Value (This_End_Straight)'Length
            Track.Point_End_Array (Straight).Adjacent.Track_Name :=
              To_Unbounded_String (Get_Value (Adjacent_Track_Straight));
            if Length (Track.Point_End_Array (Straight).Adjacent.
                         Track_Name) > 0 then
               if Get_Value (Adjacent_End_Straight)'Length = 1 then
                  Track.Point_End_Array (Straight).Adjacent.Track_End :=
                    First_Character (Get_Value (Adjacent_End_Straight));
               else
                  raise Data_Error with
                    "Invalid data for Adjacent_End_Straight";
               end if; -- Get_Value (Adjacent_End_Straight)'Length
            end if; -- Length (Track.Point_End_Array (Straight).Adjacent ...
            Track.Point_End_Array (Straight).Length :=
              Metres'Value (Get_Value (Length_Straight));
            Track.Point_End_Array (Straight).Is_Clear :=
              Boolean'Value (Get_Value (Is_Clear_Straight));
            -- Divergent end
            if Get_Value (This_End_Divergent)'Length = 1 then
               Track.Point_End_Array (Divergent).This_End :=
                 First_Character (Get_Value (This_End_Divergent));
            else
               raise Data_Error with "Invalid data for This_End_Divergent";
            end if; -- Get_Value (This_End_Divergent)'Length
            Track.Point_End_Array (Divergent).Adjacent.Track_Name :=
              To_Unbounded_String (Get_Value (Adjacent_Track_Divergent));
            if Length (Track.Point_End_Array (Divergent).Adjacent.
                         Track_Name) > 0 then
               if Get_Value (Adjacent_End_Divergent)'Length = 1 then
                  Track.Point_End_Array (Divergent).Adjacent.Track_End :=
                    First_Character (Get_Value (Adjacent_End_Divergent));
               else
                  raise Data_Error with
                    "Invalid data for Adjacent_End_Divergent";
               end if; -- Get_Value (Adjacent_End_Divergent)'Length
            end if; -- Length (Track.Point_End_Array (Divergent).Adjacent ...
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

   procedure Diamond_Track (Track_Store : in out Track_Stores.List) is

      package Tracks_CSV is new DJH.Parse_CSV (Diamond_Header);
      use Tracks_CSV;

      Track : Tracks (Diamond);

   begin -- Diamond_Track
      Read_Header (Compose (Name => Diamond_File_Name, Extension => CSV));
      Put_Line ("Reading " &
                  Compose (Name => Diamond_File_Name, Extension => CSV));
      while Next_Row loop
         begin -- record exception block
            Track.Track_Name := To_Unbounded_String (Get_Value (Track_Name));
            -- Left Straight
            if Get_Value (This_End_Left_Straight)'Length = 1 then
               Track.Diamond_End_Array (Left_Straight).This_End :=
                 First_Character (Get_Value (This_End_Left_Straight));
            else
               raise Data_Error with "Invalid data for This_End_Left_Straight";
            end if; -- Get_Value (This_End_Left_Straight)'Length = 1
            Track.Diamond_End_Array (Left_Straight).Adjacent.Track_Name :=
              To_Unbounded_String (Get_Value (Adjacent_Track_Left_Straight));
            if Length (Track.Diamond_End_Array (Left_Straight).Adjacent.
                         Track_Name) > 0 then
               if Get_Value (Adjacent_End_Left_Straight)'Length = 1 then
                  Track.Diamond_End_Array (Left_Straight).Adjacent.Track_End :=
                    First_Character (Get_Value (Adjacent_End_Left_Straight));
               else
                  raise Data_Error with
                    "Invalid data for Adjacent_End_Left_Straight";
               end if; -- Get_Value (Adjacent_End_Left_Straight)'Length
            end if; -- Length (Track.Diamond_End_Array (Left_Straight) ...
            Track.Diamond_End_Array (Left_Straight).Length :=
              Metres'Value (Get_Value (Length_Left_Straight));
            Track.Diamond_End_Array (Left_Straight).Is_Clear :=
              Boolean'Value (Get_Value (Is_Clear_Left_Straight));
            -- Right Straight
            if Get_Value (This_End_Right_Straight)'Length = 1 then
               Track.Diamond_End_Array (Right_Straight).This_End :=
                 First_Character (Get_Value (This_End_Right_Straight));
            else
               raise Data_Error with "Invalid data for This_End_Right_Straight";
            end if; -- Get_Value (This_End_Right_Straight)'Length = 1
            Track.Diamond_End_Array (Right_Straight).Adjacent.Track_Name :=
              To_Unbounded_String (Get_Value (Adjacent_Track_Right_Straight));
            if Length (Track.Diamond_End_Array (Right_Straight).Adjacent.
                         Track_Name) > 0 then
               if Get_Value (Adjacent_End_Right_Straight)'Length = 1 then
                  Track.Diamond_End_Array (Right_Straight).Adjacent.Track_End :=
                    First_Character (Get_Value (Adjacent_End_Right_Straight));
               else
                  raise Data_Error with
                    "Invalid data for Adjacent_End_Right_Straight";
               end if; -- Get_Value (Adjacent_End_Right_Straight)'Length
            end if; -- Length (Track.Diamond_End_Array (Right_Straight) ...
            Track.Diamond_End_Array (Right_Straight).Length :=
              Metres'Value (Get_Value (Length_Right_Straight));
            Track.Diamond_End_Array (Right_Straight).Is_Clear :=
              Boolean'Value (Get_Value (Is_Clear_Right_Straight));
            -- Left Cross
            if Get_Value (This_End_Left_Cross)'Length = 1 then
               Track.Diamond_End_Array (Left_Cross).This_End :=
                 First_Character (Get_Value (This_End_Left_Cross));
            else
               raise Data_Error with "Invalid data for This_End_Left_Cross";
            end if; -- Get_Value (This_End_Left_Cross)'Length = 1
            Track.Diamond_End_Array (Left_Cross).Adjacent.Track_Name :=
              To_Unbounded_String (Get_Value (Adjacent_Track_Left_Cross));
            if Length (Track.Diamond_End_Array (Left_Cross).Adjacent.
                         Track_Name) > 0 then
               if Get_Value (Adjacent_End_Left_Cross)'Length = 1 then
                  Track.Diamond_End_Array (Left_Cross).Adjacent.Track_End :=
                    First_Character (Get_Value (Adjacent_End_Left_Cross));
               else
                  raise Data_Error with
                    "Invalid data for Adjacent_End_Left_Cross";
               end if; -- Get_Value (Adjacent_End_Left_Cross)'Length
            end if; -- Length (Track.Diamond_End_Array (Left_Cross) ...
            Track.Diamond_End_Array (Left_Cross).Length :=
              Metres'Value (Get_Value (Length_Left_Cross));
            Track.Diamond_End_Array (Left_Cross).Is_Clear :=
              Boolean'Value (Get_Value (Is_Clear_Left_Cross));
            -- Right Cross
            if Get_Value (This_End_Right_Cross)'Length = 1 then
               Track.Diamond_End_Array (Right_Cross).This_End :=
                 First_Character (Get_Value (This_End_Right_Cross));
            else
               raise Data_Error with "Invalid data for This_End_Right_Cross";
            end if; -- Get_Value (This_End_Right_Cross)'Length = 1
            Track.Diamond_End_Array (Right_Cross).Adjacent.Track_Name :=
              To_Unbounded_String (Get_Value (Adjacent_Track_Right_Cross));
            if Length (Track.Diamond_End_Array (Right_Cross).Adjacent.
                         Track_Name) > 0 then
               if Get_Value (Adjacent_End_Right_Cross)'Length = 1 then
                  Track.Diamond_End_Array (Right_Cross).Adjacent.Track_End :=
                    First_Character (Get_Value (Adjacent_End_Right_Cross));
               else
                  raise Data_Error with
                    "Invalid data for Adjacent_End_Right_Cross";
               end if; -- Get_Value (Adjacent_End_Right_Cross)'Length
            end if; -- Length (Track.Diamond_End_Array (Right_Cross) ...
            Track.Diamond_End_Array (Right_Cross).Length :=
              Metres'Value (Get_Value (Length_Right_Cross));
            Track.Diamond_End_Array (Right_Cross).Is_Clear :=
              Boolean'Value (Get_Value (Is_Clear_Right_Cross));
            Track_Stores.Append (Track_Store, Track);
         exception
            when E: others =>
               Put_Line ("Row:" & Row_Number'Image & " > " & Exception_Name (E)
                         & " - " & Exception_Message (E));
         end; -- record exception block
      end loop; -- Next_Row
      Close_CSV;
   end Diamond_Track;

   procedure Switch_Diamond_Track (Track_Store : in out Track_Stores.List) is

      package Tracks_CSV is new DJH.Parse_CSV (Switch_Diamond_Header);
      use Tracks_CSV;

      Track : Tracks (Switch_Diamond);

   begin -- Switch_Diamond_Track
      Read_Header (Compose (Name =>
                              Switch_Diamond_File_Name, Extension => CSV));
      Put_Line ("Reading " &
                  Compose (Name => Switch_Diamond_File_Name, Extension => CSV));
      while Next_Row loop
         begin -- record exception block
            Track.Track_Name := To_Unbounded_String (Get_Value (Track_Name));
            Track.Diamond_Number :=
              Point_Numbers'Value (Get_Value (Diamond_Number));
            if Get_Value (Diamond_End)'Length = 1 then
               Track.Diamond_End := First_Character (Get_Value (Diamond_End));
            else
               raise Data_Error with "Invalid data for Diamond_End";
            end if; -- Get_Value (Diamond_End)'Length = 1
            Track.Has_Left_Swing_Nose :=
              Boolean'Value (Get_Value (Has_Left_Swing_Nose));
            if Track.Has_Left_Swing_Nose then
               if Get_Value (Left_Swing_Nose_End)'Length = 1 then
                  Track.Left_Swing_Nose_End :=
                    First_Character (Get_Value (Left_Swing_Nose_End));
               else
                  raise Data_Error with "Invalid data for Left_Swing_Nose_End";
               end if; -- Get_Value (Left_Swing_Nose_End)'Length = 1
            end if; -- Track.Has_Left_Swing_Nose
            Track.Has_Right_Swing_Nose :=
              Boolean'Value (Get_Value (Has_Right_Swing_Nose));
            if Track.Has_Right_Swing_Nose then
               if Get_Value (Right_Swing_Nose_End)'Length = 1 then
                  Track.Right_Swing_Nose_End :=
                    First_Character (Get_Value (Right_Swing_Nose_End));
               else
                  raise Data_Error with "Invalid data for Right_Swing_Nose_End";
               end if; -- Get_Value (Right_Swing_Nose_End)'Length = 1
            end if; -- Track.Has_Right_Swing_Nose
            Track.Diamond_LHSNC := Boolean'Value (Get_Value (Diamond_LHSNC));
            -- Left Straight
            if Get_Value (This_End_Left_Straight)'Length = 1 then
               Track.Switch_Diamond_End_Array (Left_Straight).This_End :=
                 First_Character (Get_Value (This_End_Left_Straight));
            else
               raise Data_Error with "Invalid data for This_End_Left_Straight";
            end if; -- Get_Value (This_End_Left_Straight)'Length = 1
            Track.Switch_Diamond_End_Array (Left_Straight).Adjacent.Track_Name
              := To_Unbounded_String (Get_Value (Adjacent_Track_Left_Straight));
            if Length (Track.Switch_Diamond_End_Array (Left_Straight).Adjacent.
                         Track_Name) > 0 then
               if Get_Value (Adjacent_End_Left_Straight)'Length = 1 then
                  Track.Switch_Diamond_End_Array (Left_Straight).Adjacent.
                    Track_End :=
                      First_Character (Get_Value (Adjacent_End_Left_Straight));
               else
                  raise Data_Error with
                    "Invalid data for Adjacent_End_Left_Straight";
               end if; -- Get_Value (Adjacent_End_Left_Straight)'Length
            end if; -- Length (Track.Switch_Diamond_End_Array (Left_Straight)
            Track.Switch_Diamond_End_Array (Left_Straight).Length :=
              Metres'Value (Get_Value (Length_Left_Straight));
            Track.Switch_Diamond_End_Array (Left_Straight).Is_Clear :=
              Boolean'Value (Get_Value (Is_Clear_Left_Straight));
            -- Right Straight
            if Get_Value (This_End_Right_Straight)'Length = 1 then
               Track.Switch_Diamond_End_Array (Right_Straight).This_End :=
                 First_Character (Get_Value (This_End_Right_Straight));
            else
               raise Data_Error with "Invalid data for This_End_Right_Straight";
            end if; -- Get_Value (This_End_Right_Straight)'Length = 1
            Track.Switch_Diamond_End_Array (Right_Straight).Adjacent.Track_Name
              :=
                To_Unbounded_String (Get_Value (Adjacent_Track_Right_Straight));
            if Length (Track.Switch_Diamond_End_Array (Right_Straight).Adjacent.
                         Track_Name) > 0 then
               if Get_Value (Adjacent_End_Right_Straight)'Length = 1 then
                  Track.Switch_Diamond_End_Array (Right_Straight).Adjacent.
                    Track_End :=
                      First_Character (Get_Value (Adjacent_End_Right_Straight));
               else
                  raise Data_Error with
                    "Invalid data for Adjacent_End_Right_Straight";
               end if; -- Get_Value (Adjacent_End_Right_Straight)'Length
            end if; -- Length (Track.Switch_Diamond_End_Array (Right_Straight)
            Track.Switch_Diamond_End_Array (Right_Straight).Length :=
              Metres'Value (Get_Value (Length_Right_Straight));
            Track.Switch_Diamond_End_Array (Right_Straight).Is_Clear :=
              Boolean'Value (Get_Value (Is_Clear_Right_Straight));
            -- Left Cross
            if Get_Value (This_End_Left_Cross)'Length = 1 then
               Track.Switch_Diamond_End_Array (Left_Cross).This_End :=
                 First_Character (Get_Value (This_End_Left_Cross));
            else
               raise Data_Error with "Invalid data for This_End_Left_Cross";
            end if; -- Get_Value (This_End_Left_Cross)'Length = 1
            Track.Switch_Diamond_End_Array (Left_Cross).Adjacent.Track_Name :=
              To_Unbounded_String (Get_Value (Adjacent_Track_Left_Cross));
            if Length (Track.Switch_Diamond_End_Array (Left_Cross).Adjacent.
                         Track_Name) > 0 then
               if Get_Value (Adjacent_End_Left_Cross)'Length = 1 then
                  Track.Switch_Diamond_End_Array (Left_Cross).Adjacent.Track_End
                    := First_Character (Get_Value (Adjacent_End_Left_Cross));
               else
                  raise Data_Error with
                    "Invalid data for Adjacent_End_Left_Cross";
               end if; -- Get_Value (Adjacent_End_Left_Cross)'Length
            end if; -- Length (Track.Switch_Diamond_End_Array (Left_Cross) ...
            Track.Switch_Diamond_End_Array (Left_Cross).Length :=
              Metres'Value (Get_Value (Length_Left_Cross));
            Track.Switch_Diamond_End_Array (Left_Cross).Is_Clear :=
              Boolean'Value (Get_Value (Is_Clear_Left_Cross));
            -- Right Cross
            if Get_Value (This_End_Right_Cross)'Length = 1 then
               Track.Switch_Diamond_End_Array (Right_Cross).This_End :=
                 First_Character (Get_Value (This_End_Right_Cross));
            else
               raise Data_Error with "Invalid data for This_End_Right_Cross";
            end if; -- Get_Value (This_End_Right_Cross)'Length = 1
            Track.Switch_Diamond_End_Array (Right_Cross).Adjacent.Track_Name :=
              To_Unbounded_String (Get_Value (Adjacent_Track_Right_Cross));
            if Length (Track.Switch_Diamond_End_Array (Right_Cross).Adjacent.
                         Track_Name) > 0 then
               if Get_Value (Adjacent_End_Right_Cross)'Length = 1 then
                  Track.Switch_Diamond_End_Array (Right_Cross).Adjacent.
                    Track_End :=
                      First_Character (Get_Value (Adjacent_End_Right_Cross));
               else
                  raise Data_Error with
                    "Invalid data for Adjacent_End_Right_Cross";
               end if; -- Get_Value (Adjacent_End_Right_Cross)'Length
            end if; -- Length (Track.Switch_Diamond_End_Array (Right_Cross) ...
            Track.Switch_Diamond_End_Array (Right_Cross).Length :=
              Metres'Value (Get_Value (Length_Right_Cross));
            Track.Switch_Diamond_End_Array (Right_Cross).Is_Clear :=
              Boolean'Value (Get_Value (Is_Clear_Right_Cross));
            Track_Stores.Append (Track_Store, Track);
         exception
            when E: others =>
               Put_Line ("Row:" & Row_Number'Image & " > " & Exception_Name (E)
                         & " - " & Exception_Message (E));
         end; -- record exception block
      end loop; -- Next_Row
      Close_CSV;
   end Switch_Diamond_Track;

   procedure Get (Track_Store : out Track_Stores.List) is

   begin -- Get
      Track_Stores.Clear (Track_Store);
      Plain_Track (Track_Store);
      Points_Track (Track_Store);
      Diamond_Track (Track_Store);
      Switch_Diamond_Track (Track_Store);
   end Get;

   procedure Get (Signal_Store : out Signal_Stores.Map) is

      package Signals_CSV is new DJH.Parse_CSV (Signal_Header);
      use Signals_CSV;
      use Signal_Stores;

      Signal : Signals;

   begin -- Get
      Signal_Stores.Clear (Signal_Store);
      Read_Header (Compose (Name => Signal_File_Name, Extension => CSV));
      Put_Line ("Reading " &
                  Compose (Name => Signal_File_Name, Extension => CSV));
      while Next_Row loop
         begin -- record exception block
            Signal.Is_Main := Boolean'Value (Get_Value (Is_Main));
            Signal.Is_Shunt := Boolean'Value (Get_Value (Is_Shunt));
            Signal.Replacement_Track.Track_Name :=
              To_Unbounded_String (Get_Value (Replacement_Track));
            if Get_Value (Entrance_End)'Length = 1 then
               Signal.Replacement_Track.Track_End :=
                 First_Character (Get_Value (Entrance_End));
            else
               raise Data_Error with "invalid data for Entrence_End";
            end if; --  Get_Value (Entrance_End)'Length = 1
            if Contains (Signal_Store,
                         To_Unbounded_String (Get_Value (Signal_Number))) then
               raise Data_Error with "Repeated signal number " &
                 Get_Value (Signal_Number);
            else
               Insert (Signal_Store,
                       To_Unbounded_String (Get_Value (Signal_Number)),
                       Signal);
            end if; -- Contains (Signal_Store, ...
         exception
            when E: others =>
               Put_Line ("Row:" & Row_Number'Image & " > " & Exception_Name (E)
                         & " - " & Exception_Message (E));
         end; -- record exception block
      end loop; -- Next_Row
      Close_CSV;
   end Get;

   procedure Get (Route_Store : out Route_Stores.Map) is

      package Route_CSV is new DJH.Parse_CSV (Route_Header);
      use Route_CSV;
      use Route_Stores;

      Route : Routes;

   begin -- Get
      Route_Stores.Clear (Route_Store);
      Read_Header (Compose (Name => Route_File_Name, Extension => CSV));
      Put_Line ("Reading " &
                  Compose (Name => Route_File_Name, Extension => CSV));
      while Next_Row loop
         begin -- record exception block
            Route.Entrance_Signal :=
              To_Unbounded_String (Get_Value (Entrance_Signal));
            Route.Exit_Signal := To_Unbounded_String (Get_Value (Exit_Signal));
            Route.Route_Class := Route_Classes'Value (Get_Value (Route_Class));
            if Contains (Route_Store,
                         To_Unbounded_String (Get_Value (Route_Name))) then
               raise Data_Error with "Repeated route name " &
                 Get_Value (Route_Name);
            else
               Insert (Route_Store,
                       To_Unbounded_String (Get_Value (Route_Name)),
                       Route);
            end if; -- Contains (Route_Store, ...
         exception
            when E: others =>
               Put_Line ("Row:" & Row_Number'Image & " > " & Exception_Name (E)
                         & " - " & Exception_Message (E));
         end; -- record exception block
      end loop; -- Next_Row
      Close_CSV;
   end Get;

end Get_Data;
