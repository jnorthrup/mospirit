##
## Auto Generated makefile by CodeLite IDE
## any manual changes will be erased      
##
## Debug
ProjectName            :=mlite
ConfigurationName      :=Debug
WorkspacePath          := "/home/jim/work/mlite"
ProjectPath            := "/home/jim/work/mlite"
IntermediateDirectory  :=./Debug
OutDir                 := $(IntermediateDirectory)
CurrentFileName        :=
CurrentFilePath        :=
CurrentFileFullPath    :=
User                   :=jim
Date                   :=12/31/13
CodeLitePath           :="/home/jim/.codelite"
LinkerName             :=clang++ 
SharedObjectLinkerName :=clang++ -shared -fPIC
ObjectSuffix           :=.o
DependSuffix           :=
PreprocessSuffix       :=.o.i
DebugSwitch            :=-gstab
IncludeSwitch          :=-I
LibrarySwitch          :=-l
OutputSwitch           :=-o 
LibraryPathSwitch      :=-L
PreprocessorSwitch     :=-D
SourceSwitch           :=-c 
OutputFile             :=$(IntermediateDirectory)/$(ProjectName)
Preprocessors          :=
ObjectSwitch           :=-o 
ArchiveOutputSwitch    := 
PreprocessOnlySwitch   :=-E 
ObjectsFileList        :="mlite.txt"
PCHCompileFlags        :=
MakeDirCommand         :=mkdir -p
LinkOptions            :=  
IncludePath            :=  $(IncludeSwitch). $(IncludeSwitch). 
IncludePCH             := 
RcIncludePath          := 
Libs                   := 
ArLibs                 :=  
LibPath                := $(LibraryPathSwitch). 

##
## Common variables
## AR, CXX, CC, AS, CXXFLAGS and CFLAGS can be overriden using an environment variables
##
AR       := llvm-ar-3.2 rcus
CXX      := clang++
CC       := clang
CXXFLAGS :=  -std=c++11 -g -O0 -Wall $(Preprocessors) 
CFLAGS   :=  -std=c11  -g -O0 -Wall $(Preprocessors)
ASFLAGS  := 
AS       := llvm-as


##
## User defined environment variables
##
CodeLiteDir:=/usr/share/codelite
Objects0=$(IntermediateDirectory)/mlite$(ObjectSuffix) 



Objects=$(Objects0) 

##
## Main Build Targets 
##
.PHONY: all clean PreBuild PrePreBuild PostBuild
all: $(OutputFile)

$(OutputFile): $(IntermediateDirectory)/.d $(Objects) 
	@$(MakeDirCommand) $(@D)
	@echo "" > $(IntermediateDirectory)/.d
	@echo $(Objects0)  > $(ObjectsFileList)
	$(LinkerName) $(OutputSwitch)$(OutputFile) @$(ObjectsFileList) $(LibPath) $(Libs) $(LinkOptions)

$(IntermediateDirectory)/.d:
	@test -d ./Debug || $(MakeDirCommand) ./Debug

PreBuild:


##
## Objects
##
$(IntermediateDirectory)/mlite$(ObjectSuffix): mlite.cpp 
	$(CXX) $(IncludePCH) $(SourceSwitch) "/home/jim/work/mlite/mlite.cpp" $(CXXFLAGS) $(ObjectSwitch)$(IntermediateDirectory)/mlite$(ObjectSuffix) $(IncludePath)
$(IntermediateDirectory)/mlite$(PreprocessSuffix): mlite.cpp
	@$(CXX) $(CXXFLAGS) $(IncludePCH) $(IncludePath) $(PreprocessOnlySwitch) $(OutputSwitch) $(IntermediateDirectory)/mlite$(PreprocessSuffix) "mlite.cpp"

##
## Clean
##
clean:
	$(RM) $(IntermediateDirectory)/mlite$(ObjectSuffix)
	$(RM) $(IntermediateDirectory)/mlite$(DependSuffix)
	$(RM) $(IntermediateDirectory)/mlite$(PreprocessSuffix)
	$(RM) $(OutputFile)
	$(RM) ".build-debug/mlite"


