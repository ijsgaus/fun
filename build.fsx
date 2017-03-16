// include Fake lib
#r @"packages/build/FAKE/tools/FakeLib.dll"
open Fake

// Properties
let buildDir = "./build/"


Target "Clean" (fun _ ->
    CleanDir buildDir
)

// Default target
Target "Default" (fun _ ->
    trace "Hello World from FAKE"
)



Target "Restore" (fun _ ->
    DotNetCli.Restore (fun p ->
                            { p with
                                Project = "Fun.sln"
                            })
)

Target "BuildApp" (fun _ ->
    (*!! "*.sln"
      |> MSBuildRelease buildDir "Build"
      |> Log "AppBuild-Output: "*)
    
    DotNetCli.Build (fun p ->
                         { p with
                                Configuration = "Debug"
                                Project = "Fun.sln"
                         })
)

// Dependencies
"Clean"
  ==> "Restore"
  ==> "BuildApp"
  ==> "Default"

// start build
RunTargetOrDefault "Default"
