with Hera.Corporations;
with Hera.Colonies;

package Hera.Facilities.Updates is

   procedure Execute_Production
     (Facility : Hera.Facilities.Facility_Type;
      Employer : not null access constant
        Hera.Colonies.Employer_Interface'Class;
      Stock    : Hera.Commodities.Has_Stock_Interface'Class;
      Colony   : Hera.Colonies.Colony_Type;
      Owner    : Hera.Corporations.Corporation_Type;
      Queue    : in out Production_Queue);

end Hera.Facilities.Updates;
