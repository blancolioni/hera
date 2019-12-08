with WL.Images.Bitmaps;
with WL.String_Maps;

package body Hera.Color.Palettes is

   package Palette_Maps is
     new WL.String_Maps (Palette_Type);

   Palette_Map : Palette_Maps.Map;

   ---------------
   -- Get_Color --
   ---------------

   function Get_Color (Palette : Palette_Type; Value : Real) return Hera_Color
   is
      Position : constant Color_Maps.Cursor :=
                   Palette.Map.Floor (Value);
   begin
      if Color_Maps.Has_Element (Position) then
         return Color_Maps.Element (Position);
      else
         return Palette.Map.First_Element;
      end if;
   end Get_Color;

   -----------------
   -- Get_Palette --
   -----------------

   function Get_Palette (Name : String) return Palette_Type is
   begin
      return Palette_Map.Element (Name);
   end Get_Palette;

   ------------------
   -- Load_Palette --
   ------------------

   procedure Load_Palette
     (Name : String; Low, High : Real; Bitmap_Path : String)
   is
      Palette : Palette_Type;
      Reader            : WL.Images.Bitmaps.Bitmap_Image_Reader;
      Image             : WL.Images.Image_Type;
   begin
      Reader.Read (Bitmap_Path, Image);

      for X in 1 .. Image.Width loop
         declare
            Color : constant WL.Images.Image_Color :=
                      Image.Color (X, 1);
         begin
            Palette.Map.Insert
              ((Real (X) - 1.0) * (High - Low)
               / (Real (Image.Width) - 1.0) + Low,
               (Real (Color.Red) / 255.0,
                Real (Color.Green) / 255.0,
                Real (Color.Blue) / 255.0,
                1.0));
         end;
      end loop;

      Palette_Map.Insert (Name, Palette);
   end Load_Palette;

end Hera.Color.Palettes;
