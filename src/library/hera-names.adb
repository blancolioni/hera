with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Vectors;

with WL.Random;

package body Hera.Names is

   package Name_Part_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   type Name_Part_Position is (Prefix, Middle, Suffix);

   type Position_Array is
     array (Name_Part_Position) of Name_Part_Vectors.Vector;

   type Name_Type is (Colony_Name, Corporation_Name);

   Parts : array (Name_Type) of Position_Array;

   function Join_Parts
     (Prefix, Middle, Suffix : String)
      return String;

   function Random_Name
     (Entity : Name_Type)
      return String;

   ---------------------
   -- Configure_Names --
   ---------------------

   procedure Configure_Names
     (Config : Tropos.Configuration)
   is

      procedure Configure
        (Field_Name : String;
         Name       : Name_Type);

      ---------------
      -- Configure --
      ---------------

      procedure Configure
        (Field_Name : String;
         Name       : Name_Type)
      is
         function Position_Name
           (Position : Name_Part_Position)
            return String
         is (Ada.Characters.Handling.To_Lower
             (Position'Image));
      begin
         for Position in Name_Part_Position loop
            for Name_Config of
              Config.Child (Field_Name)
              .Child (Position_Name (Position))
            loop
               Parts (Name) (Position).Append
                 (Name_Config.Config_Name);
            end loop;
         end loop;
      end Configure;

   begin
      Configure ("corporation", Corporation_Name);
      Configure ("colony", Colony_Name);
   end Configure_Names;

   ----------------
   -- Join_Parts --
   ----------------

   function Join_Parts
     (Prefix, Middle, Suffix : String)
      return String
   is
      function Is_Upper (Ch : Character) return Boolean
        renames Ada.Characters.Handling.Is_Upper;
   begin
      return Prefix
        & (if Prefix = "" then "" else " ")
        & Middle
        & (if Suffix /= "" and then Is_Upper (Suffix (Suffix'First))
           then " "
           else "")
        & Suffix;
   end Join_Parts;

   ------------------------
   -- Random_Colony_Name --
   ------------------------

   function Random_Colony_Name return String is
   begin
      return Random_Name (Colony_Name);
   end Random_Colony_Name;

   ---------------------------
   -- Random_Corporate_Name --
   ---------------------------

   function Random_Corporate_Name return String is
   begin
      return Random_Name (Corporation_Name);
   end Random_Corporate_Name;

   function Random_Name
     (Entity : Name_Type)
      return String
   is
      Prefix_Vector : Name_Part_Vectors.Vector renames
        Parts (Entity) (Prefix);
      Middle_Vector : Name_Part_Vectors.Vector renames
        Parts (Entity) (Middle);
      Suffix_Vector : Name_Part_Vectors.Vector renames
        Parts (Entity) (Suffix);
      Prefix_Index  : constant Positive :=
        WL.Random.Random_Number (1, Prefix_Vector.Last_Index);
      Middle_Index  : constant Positive :=
        WL.Random.Random_Number (1, Middle_Vector.Last_Index);
      Suffix_Index  : constant Positive :=
        WL.Random.Random_Number (1, Suffix_Vector.Last_Index);
      Prefix        : constant String := Prefix_Vector.Element (Prefix_Index);
      Middle        : constant String := Middle_Vector.Element (Middle_Index);
      Suffix        : constant String := Suffix_Vector.Element (Suffix_Index);
   begin
      return Join_Parts (Prefix, Middle, Suffix);
   end Random_Name;

end Hera.Names;
