with Ada.Containers.Vectors;

package body Hera.Stars is

   package Star_Vectors is
     new Ada.Containers.Vectors (Positive, Star_Type);

   Vector : Star_Vectors.Vector;

   ---------------
   -- Save_Star --
   ---------------

   function Save_Star (Star : Root_Star_Type'Class) return Star_Type is
      New_Star : constant Star_Type :=
                   Star_Type
                     (Star.Save_Object);
   begin
      Vector.Append (New_Star);
      return New_Star;
   end Save_Star;

end Hera.Stars;
