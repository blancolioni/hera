with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with Hera.Real_Images;

package body Hera.Reports is

   type Field_Alignment is (Left, Centre, Right);

   type Heading_Field (Length : Natural) is
      record
         Heading : String (1 .. Length);
         Start   : Positive;
         Width   : Positive;
      end record;

   package Heading_Field_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Heading_Field);

   Headings           : Heading_Field_Vectors.Vector;
   Next_Heading_Start : Positive := 1;
   Next_Data_Field    : Positive := 1;
   First_Row          : Boolean  := True;

   procedure Put_Field
     (Value     : String;
      Alignment : Field_Alignment);

   ----------
   -- Data --
   ----------

   procedure Data (Text : String) is
   begin
      Put_Field (Text, Left);
   end Data;

   ----------
   -- Data --
   ----------

   procedure Data (Value : Integer) is
   begin
      Put_Field
        (Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Left),
         Right);
   end Data;

   ----------
   -- Data --
   ----------

   procedure Data (Value : Real) is
   begin
      Put_Field
        (Hera.Real_Images.Approximate_Image (Value), Right);
   end Data;

   ----------
   -- Data --
   ----------

   procedure Data (Value : Boolean) is
   begin
      Put_Field
        ((if Value then "yes" else "no"), Centre);
   end Data;

   ----------
   -- Data --
   ----------

   procedure Data (Value : Hera.Money.Money_Type) is
   begin
      Put_Field
        (Hera.Money.Show (Value), Right);
   end Data;

   ----------------
   -- End_Report --
   ----------------

   procedure End_Report is
   begin
      Headings.Clear;
      Next_Heading_Start := 1;
      Next_Data_Field    := 1;
      First_Row := True;
   end End_Report;

   -------------
   -- Heading --
   -------------

   procedure Heading
     (Text  : String;
      Width : Positive)
   is
      Spaces : constant String (1 .. Integer'Max (Width - Text'Length, 1)) :=
        (others => ' ');
   begin
      Ada.Text_IO.Put
        (Text (Text'First ..
             Text'First + Natural'Min (Width, Text'Length) - 1));
      Ada.Text_IO.Put (Spaces);

      Headings.Append (Heading_Field'
                         (Length  => Text'Length,
                          Heading => Text,
                          Start   => Next_Heading_Start,
                          Width   => Width));
      Next_Heading_Start := Next_Heading_Start + Width;
      First_Row := True;

   end Heading;

   ---------------
   -- Put_Field --
   ---------------

   procedure Put_Field
     (Value     : String;
      Alignment : Field_Alignment)
   is
      Field  : constant Heading_Field := Headings.Element (Next_Data_Field);
      Width  : constant Positive := Field.Width;
      Format : String (1 .. Width - 1) := (others => ' ');
   begin
      if First_Row then
         Ada.Text_IO.New_Line;
         First_Row := False;
      end if;

      if Value'Length >= Format'Length then
         Format := Value (Value'First .. Value'First + Format'Length - 1);
      else
         declare
            Start, Finish : Positive;
         begin
            case Alignment is
               when Left =>
                  Start := 1;
                  Finish := Value'Length;
               when Centre =>
                  Start := Format'Length / 2 - Value'Length / 2 + 1;
                  Finish := Start + Value'Length - 1;
               when Right =>
                  Start := Format'Last - Value'Length + 1;
                  Finish := Format'Last;
            end case;
            Format (Start .. Finish) := Value;
         end;
      end if;

      Ada.Text_IO.Put (Format);
      if Next_Data_Field = Headings.Last_Index then
         Ada.Text_IO.New_Line;
         Next_Data_Field := 1;
      else
         Ada.Text_IO.Put (' ');
         Next_Data_Field := Next_Data_Field + 1;
      end if;

   end Put_Field;

   ------------------
   -- Start_Report --
   ------------------

   procedure Start_Report
     (Name : String)
   is
      Lines : constant String (Name'Range) := (others => '-');
   begin
      Ada.Text_IO.Put_Line (Name);
      Ada.Text_IO.Put_Line (Lines);

      Headings.Clear;
      Next_Heading_Start := 1;
      Next_Data_Field    := 1;
      First_Row := True;
   end Start_Report;

end Hera.Reports;
