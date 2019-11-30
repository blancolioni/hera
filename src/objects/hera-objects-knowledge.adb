package body Hera.Objects.Knowledge is

   ---------------------
   -- Knowledge_Level --
   ---------------------

   function Knowledge_Level
     (Object_Knowledge : Object_Knowledge_Type;
      Object           : Root_Hera_Object'Class)
      return Hera.Knowledge.Knowledge_Level_Type
   is
   begin
      return (if Object_Knowledge.Contains (Object.Identifier)
              then Object_Knowledge.Element (Object.Identifier)
              else Hera.Knowledge.None);
   end Knowledge_Level;

   ----------------------
   -- Update_Knowledge --
   ----------------------

   procedure Update_Knowledge
     (Object_Knowledge : in out Object_Knowledge_Type;
      Object           :    not null access constant Root_Hera_Object'Class;
      Knowledge_Level  :        Hera.Knowledge.Knowledge_Level_Type)
   is
   begin
      if not Object_Knowledge.Contains (Object.Identifier) then
         Object_Knowledge.Insert (Object.Identifier, Knowledge_Level);
      else
         Object_Knowledge.Replace (Object.Identifier, Knowledge_Level);
      end if;
   end Update_Knowledge;

end Hera.Objects.Knowledge;
