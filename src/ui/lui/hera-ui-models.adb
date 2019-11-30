with WL.String_Maps;

package body Hera.UI.Models is

   package Model_Maps is
     new WL.String_Maps (Lui.Models.Object_Model, Lui.Models."=");

   Model_Map : Model_Maps.Map;

   ------------------
   -- After_Render --
   ------------------

--     overriding procedure After_Render
--       (Model    : in out Root_Hera_Model;
--        Renderer : in out Lui.Rendering.Root_Renderer'Class)
--     is null;

   -------------------
   -- Before_Render --
   -------------------

--     overriding procedure Before_Render
--       (Model    : in out Root_Hera_Model;
--        Renderer : in out Lui.Rendering.Root_Renderer'Class)
--     is null;

   ---------------
   -- Get_Model --
   ---------------

   function Get_Model
     (Key     : String)
      return Lui.Models.Object_Model
   is
   begin
      return Model_Map.Element (Key);
   end Get_Model;

   ----------------
   -- Have_Model --
   ----------------

   function Have_Model
     (Key     : String)
      return Boolean
   is
   begin
      return Model_Map.Contains (Key);
   end Have_Model;

   ----------------
   -- Save_Model --
   ----------------

   procedure Save_Model
     (Model    : not null access Lui.Models.Root_Object_Model'Class;
      Class_Id : String)
   is
   begin
      Model_Map.Insert
        (Class_Id,
         Model);
   end Save_Model;

   -------------------
   -- To_Lui_Color --
   -------------------

   function To_Lui_Color
     (Color : Hera.Color.Hera_Color)
      return Lui.Colors.Color_Type
   is
      use Lui.Colors;
   begin
      return Apply_Alpha
        (To_Color
           (Red   => Color_Byte (Color.Red * 255.0),
            Green => Color_Byte (Color.Green * 255.0),
            Blue  => Color_Byte (Color.Blue * 255.0)),
         Lui.Unit_Real (Color.Alpha));
   end To_Lui_Color;

end Hera.UI.Models;
