with Hera.Colonies;

package Hera.Managers.Colonies is

   function Colony_Manager
     (Corporation : Hera.Corporations.Corporation_Type;
      Colony      : Hera.Colonies.Colony_Type)
      return Manager_Type;

end Hera.Managers.Colonies;
