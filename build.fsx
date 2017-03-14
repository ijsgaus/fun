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



Target "BuildApp" (fun _ ->
    !! "*.sln"
      |> MSBuildRelease buildDir "Build"
      |> Log "AppBuild-Output: "
)

// Dependencies
"Clean"
  ==> "BuildApp"
  ==> "Default"

// start build
RunTargetOrDefault "Default"
