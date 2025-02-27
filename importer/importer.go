package importer

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

type ModuleInfo struct {
	Name     string
	FilePath string
	Content  string
	Imports  []string
}

type Importer struct {
	processedModules map[string]*ModuleInfo
	moduleStack      []string // For circular dependency detection
}

func New() *Importer {
	return &Importer{
		processedModules: make(map[string]*ModuleInfo),
		moduleStack:      make([]string, 0),
	}
}

func (i *Importer) ProcessFile(filePath string) (string, error) {
	absolutePath, err := filepath.Abs(filePath)
	if err != nil {
		return "", err
	}

	// Start with empty result
	result := ""
	moduleInfo, err := i.processModule(absolutePath)
	if err != nil {
		return "", err
	}

	// Combine all imported modules' content
	for _, imp := range i.processedModules {
		if imp.Name != moduleInfo.Name {
			result += imp.Content + "\n"
		}
	}

	// Add main file's content last
	result += moduleInfo.Content

	debugPath := filepath.Join(filepath.Dir(absolutePath), "debug_output.cl")
	if err := os.WriteFile(debugPath, []byte(result), 0644); err != nil {
		return "", fmt.Errorf("failed to write debug file: %v", err)
	}
	return result, nil
}

func (i *Importer) processModule(filePath string) (*ModuleInfo, error) {
	// Check if already processed
	if info, exists := i.processedModules[filePath]; exists {
		return info, nil
	}

	// Read file content
	content, err := os.ReadFile(filePath)
	if err != nil {
		return nil, err
	}

	// Parse module declaration and imports
	moduleName, imports, processedContent, err := i.parseFileContent(string(content))
	if err != nil {
		return nil, err
	}

	// If no module name found, use filename without extension
	if moduleName == "" {
		base := filepath.Base(filePath)
		moduleName = strings.TrimSuffix(base, filepath.Ext(base))
	}

	// Check for circular dependencies
	if i.isInStack(moduleName) {
		return nil, fmt.Errorf("circular dependency detected for module %s", moduleName)
	}

	// Create module info
	moduleInfo := &ModuleInfo{
		Name:     moduleName,
		FilePath: filePath,
		Content:  processedContent,
		Imports:  imports,
	}

	// Add to stack for circular dependency checking
	i.moduleStack = append(i.moduleStack, moduleName)

	// Process imports recursively
	dir := filepath.Dir(filePath)
	for _, imp := range imports {
		importPath := filepath.Join(dir, imp+".cl")
		_, err := i.processModule(importPath)
		if err != nil {
			return nil, err
		}
	}

	// Remove from stack
	i.moduleStack = i.moduleStack[:len(i.moduleStack)-1]

	// Store processed module
	i.processedModules[filePath] = moduleInfo

	return moduleInfo, nil
}

func (i *Importer) parseFileContent(content string) (string, []string, string, error) {
	var moduleName string
	imports := make([]string, 0)
	lines := strings.Split(content, "\n")
	processedLines := make([]string, 0, len(lines))

	for _, line := range lines {
		trimmed := strings.TrimSpace(line)

		if strings.HasPrefix(trimmed, "module ") {
			moduleName = strings.TrimSpace(strings.TrimPrefix(trimmed, "module"))
			moduleName = strings.TrimSuffix(moduleName, ";")
			continue
		}

		if strings.HasPrefix(trimmed, "import ") {
			imp := strings.TrimSpace(strings.TrimPrefix(trimmed, "import"))
			imp = strings.TrimSuffix(imp, ";")
			imports = append(imports, imp)
			continue
		}

		processedLines = append(processedLines, line)
	}

	return moduleName, imports, strings.Join(processedLines, "\n"), nil
}

func (i *Importer) isInStack(moduleName string) bool {
	for _, m := range i.moduleStack {
		if m == moduleName {
			return true
		}
	}
	return false
}
