package Hera.Profiling is

   procedure Start (Name : String);
   procedure Stop (Name : String);

   procedure Iterate_Profiles
     (Process : not null access
        procedure (Name : String;
                   Timing : Duration;
                   Count  : Positive));

   procedure Report_Profiles;

end Hera.Profiling;
