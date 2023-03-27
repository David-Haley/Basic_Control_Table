-- Package Defines the CSV file headers.

-- Author    : David Haley
-- Created   : 27/03/2023
-- Last Edit : 27/03/2023

package File_Heders is

   CSV : constant String := "csv";

   Track_File_Name : constant String := "Tracks";

   type Track_Header is
     (Track_Name, Left_End, Right_End,
      Adjacent_Left_Track, Adjacent_Left_End,
      Adjacent_Right_Track, Adjacent_Right_End, Length);

   Points_File_Name : constant String := "Points";

   type Points_Header is
     (Track_Name, Points_Number, Is_Single_Ended, Points_End,
      Has_Swing_Nose, Swing_Nose_End, Normal_Is_Straight, Points_LHSNC,
      This_End_Facing, Adjacent_Track_Facing, Adjacent_End_Facing,
      Length_Facing, Is_Clear_Facing, This_End_Straight,
      Adjacent_Track_Straight, Adjacent_End_Straight,
      Length_Straight, Is_Clear_Straight, This_End_Divergent,
      Adjacent_Track_Divergent, Adjacent_End_Divergent,
      Length_Divergent, Is_Clear_Divergent);

   Diamond_File_Name : constant String := "Diamond";

   type Diamond_Header is
     (Track_Name, This_End_Left_Straight, Adjacent_Track_Left_Straight,
      Adjacent_End_Left_Straight, Length_Left_Straight,
      Is_Clear_Left_Straight, This_End_Right_Straight,
      Adjacent_Track_Right_Straight, Adjacent_End_Right_Straight,
      Length_Right_Straight, Is_Clear_Right_Straight,
      This_End_Left_Cross, Adjacent_Track_Left_Cross,
      Adjacent_End_Left_Cross, Length_Left_Cross,
      Is_Clear_Left_Cross, This_End_Right_Cross, Adjacent_Track_Right_Cross,
      Adjacent_End_Right_Cross, Length_Right_Cross, Is_Clear_Right_Cross);

   Switch_Diamond_File_Name : constant String := "Switch_Diamond";

   type Switch_Diamond_Header is
     (Track_Name, Diamond_Number, Diamond_End, Has_Left_Swing_Nose,
      Left_Swing_Nose_End, Has_Right_Swing_Nose, Right_Swing_Nose_End,
      Diamond_LHSNC, This_End_Left_Straight, Adjacent_Track_Left_Straight,
      Adjacent_End_Left_Straight, Length_Left_Straight,
      Is_Clear_Left_Straight, This_End_Right_Straight,
      Adjacent_Track_Right_Straight, Adjacent_End_Right_Straight,
      Length_Right_Straight, Is_Clear_Right_Straight,
      This_End_Left_Cross, Adjacent_Track_Left_Cross,
      Adjacent_End_Left_Cross, Length_Left_Cross,
      Is_Clear_Left_Cross, This_End_Right_Cross, Adjacent_Track_Right_Cross,
      Adjacent_End_Right_Cross, Length_Right_Cross, Is_Clear_Right_Cross);

   Signal_File_Name : constant String := "Signals";

   type Signal_Header is
     (Signal_Number, Is_Main, Is_Shunt, Replacement_Track, Entrence_End);

   Route_File_Name : constant String := "Routes";

   type Route_Header is
     (Route_Name, Entrance_Signal, Exit_Signal, Route_Class);


end File_Heders;
