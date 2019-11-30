with Hera.Names;

with Hera.Managers;

package body Hera.Corporations.Configure is

   ------------
   -- Create --
   ------------

   function Create
     (Planet     : Hera.Planets.Planet_Type;
      Start_Cash : Hera.Money.Money_Type)
      return Corporation_Type
   is
      Name          : constant String :=
        Hera.Names.Random_Corporate_Name;
   begin
      return Corporation : constant Corporation_Type :=
        New_Corporation
          (Name       => Name,
           Planet     => Planet,
           Cash       => Start_Cash,
           Autoplayer => True)
      do
         Corporation.Log ("founded on " & Planet.Name);
         if Corporation.Autoplayer then
            Hera.Managers.Start_Manager (Corporation);
         end if;
      end return;
   end Create;

end Hera.Corporations.Configure;
