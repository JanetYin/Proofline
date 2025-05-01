import * as monaco from 'monaco-editor';
import { ProofState, Hole } from '../models/ProofState';
import prooflineApi from '../api/prooflineApi';
import { parseErrorMessage, formatErrorAsHTML } from '../utils/parserUtils';

// Define token types for syntax highlighting
export enum TokenType {
  Keyword = 'keyword',
  Type = 'type',
  Variable = 'variable',
  Constructor = 'constructor',
  Comment = 'comment',
  Error = 'error'
}

export class Editor {
    private editor: monaco.editor.IStandaloneCodeEditor | null = null;
    private errorDecorations: string[] = [];
    private throttleTimer: number | null = null;
    private onHoleSelect: (hole: Hole) => void;
    private onError: ((error: string | null) => void) | null = null;
    private proofState: ProofState | null = null;
    private loadingOverlay: HTMLElement | null = null;
    private autoCheckEnabled: boolean = false;

    constructor(
        containerId: string, 
        onHoleSelect: (hole: Hole) => void,
        onError?: (error: string | null) => void
      ) {
        this.onHoleSelect = onHoleSelect;
        this.onError = onError || null;
        this.setupEditor(containerId);
        this.createLoadingOverlay(containerId);
      }
    
    /**
     * Create a loading overlay for the editor
     */
    private createLoadingOverlay(containerId: string): void {
      const container = document.getElementById(containerId);
      if (!container) return;
      
      this.loadingOverlay = document.createElement('div');
      this.loadingOverlay.className = 'editor-loading-overlay';
      this.loadingOverlay.innerHTML = `
        <div class="spinner"></div>
        <div class="loading-text">Processing...</div>
      `;
      this.loadingOverlay.style.display = 'none';
      
      container.appendChild(this.loadingOverlay);
    }
    
    /**
     * Show loading state
     */
    public showLoading(isLoading: boolean): void {
      if (this.editor) {
        this.editor.updateOptions({ readOnly: isLoading });
      }
      
      if (this.loadingOverlay) {
        this.loadingOverlay.style.display = isLoading ? 'flex' : 'none';
      }
    }

  /**
   * Set up the Monaco editor instance with controlled auto-check
   * @param containerId The ID of the container element
   */
  private setupEditor(containerId: string): void {
    // Register custom language for Proofline
    monaco.languages.register({ id: 'proofline' });
    
    // Define custom theme
    monaco.editor.defineTheme('prooflineEnhancedTheme', {
      base: 'vs',
      inherit: true,
      rules: [
        { token: TokenType.Keyword, foreground: '0550ae', fontStyle: 'bold' },
        { token: TokenType.Type, foreground: '267f99', fontStyle: 'bold' },
        { token: TokenType.Variable, foreground: '001080' },
        { token: TokenType.Constructor, foreground: '795e26', fontStyle: 'bold' },
        { token: TokenType.Comment, foreground: '008000', fontStyle: 'italic' },
        { token: TokenType.Error, foreground: 'd32f2f', fontStyle: 'italic underline' }
      ],
      colors: {
        'editor.foreground': '#333333',
        'editor.background': '#f8f8f8',
        'editorCursor.foreground': '#007acc',
        'editor.lineHighlightBackground': '#f0f0f0',
        'editorLineNumber.foreground': '#999999',
        'editorLineNumber.activeForeground': '#333333',
        'editor.selectionBackground': '#add6ff',
        'editor.inactiveSelectionBackground': '#e5ebf1',
        'editorIndentGuide.background': '#d3d3d3',
        'editor.findMatchBackground': '#ffcb6b50',
        'editor.findMatchHighlightBackground': '#f8e71c40',
        'editorWidget.background': '#f8f8f8',
        'editorWidget.border': '#c8c8c8',
        'editorHoverWidget.background': '#f8f8f8',
        'editorHoverWidget.border': '#c8c8c8',
        'editorSuggestWidget.background': '#f8f8f8',
        'editorSuggestWidget.border': '#c8c8c8',
        'editorSuggestWidget.selectedBackground': '#e8e8e8',
        'editorSuggestWidget.highlightForeground': '#0066bf'
      }
    });

    // Configure language tokens
    monaco.languages.setMonarchTokensProvider('proofline', {
      tokenizer: {
        root: [
          [/let|in|def|type|rec|match|with|fun|λ|Π|\\/i, TokenType.Keyword],
          [/[A-Z][a-zA-Z0-9_]*/, TokenType.Type],
          [/[a-z][a-zA-Z0-9_]*/, TokenType.Variable],
          [/--.*$/, TokenType.Comment]
        ]
      }
    });

    // Create editor
    this.editor = monaco.editor.create(document.getElementById(containerId)!, {
      value: '',
      language: 'proofline',
      theme: 'prooflineEnhancedTheme',
      automaticLayout: true,
      minimap: { enabled: true },
      lineNumbers: 'on',
      scrollBeyondLastLine: false,
      fontSize: 14,
      fontFamily: 'Source Code Pro, Consolas, monospace',
      tabSize: 2,
      renderWhitespace: 'boundary',
      cursorBlinking: 'smooth',
      cursorSmoothCaretAnimation: 'on'
    });
    
    this.editor.onDidChangeModelContent((e) => {
      // Detect content changes and notify application with explicit source
      const currentContent = this.editor?.getValue() || '';
      const hasContent = currentContent.trim() !== '';
      
      // Dispatch event for any content change (typing, pasting, etc.)
      const event = new CustomEvent('editor-content-changed', {
        detail: { 
          hasContent,
          changeSource: 'user-input',
          contentPreview: currentContent.substring(0, 50) + (currentContent.length > 50 ? '...' : '')
        }
      });
      document.dispatchEvent(event);
      
      // Handle auto-check if enabled
      if (this.autoCheckEnabled) {
        this.scheduleTypeCheck();
      }
    });
  }

  /**
   * Enable or disable automatic type checking
   */
  public setAutoCheck(enabled: boolean): void {
    this.autoCheckEnabled = enabled;
  }
  
  /**
   * Schedule a type check with throttling
   */
  private scheduleTypeCheck(): void {
    if (this.throttleTimer !== null) {
      window.clearTimeout(this.throttleTimer);
    }
    
    this.throttleTimer = window.setTimeout(() => {
      this.typeCheck();
    }, 500); // Debounce for 500ms
  }
  
  /**
   * Manually type check the current content
   */
  public async manualTypeCheck(): Promise<void> {
    if (!this.editor) return;
    await this.typeCheck();
  }
  
  /**
   * Type check the current editor content
   */
  public async typeCheck(): Promise<void> {
    if (!this.editor) return;
    
    const code = this.editor.getValue();
    if (!code.trim()) {
      this.showError("Please enter code");
      return;
    }
    
    this.showLoading(true);
    try {
      const response = await prooflineApi.checkProof(code);
      
      if (response.success && response.state) {
        // Make sure the script in the state matches what's in the editor
        response.state.script = code;
        this.proofState = JSON.parse(JSON.stringify(response.state));
        this.clearError();
      } else {
        // Parse error message and try to locate the error position
        const errorInfo = parseErrorMessage(response.message || "Unknown error");
        this.showError(errorInfo.message, errorInfo.line, errorInfo.column);
      }
    } catch (error) {
      console.error('Type check failed:', error);
      this.showError(error.toString());
    } finally {
      this.showLoading(false);
    }
  }
    
  private showError(message: string, line?: number, column?: number): void {
    if (!this.editor) return;
    
    const model = this.editor.getModel();
    if (!model) return;
    
    this.errorDecorations = this.editor.deltaDecorations(this.errorDecorations, []);
    
    let formattedMessage = message;
    
    if (message.includes("(stdin):") && message.includes(" | ")) {
      formattedMessage = formatErrorAsHTML(message);
    }
    
    if (line !== undefined) {
      const startColumn = column || 1;
      const endColumn = column ? column + 1 : model.getLineMaxColumn(line);
      
      this.errorDecorations = this.editor.deltaDecorations([], [{
        range: new monaco.Range(line, startColumn, line, endColumn),
        options: {
          className: 'syntax-error',
          hoverMessage: { value: formattedMessage },
          inlineClassName: 'syntax-error-inline',
          stickiness: monaco.editor.TrackedRangeStickiness.NeverGrowsWhenTypingAtEdges,
          glyphMarginClassName: 'error-glyph',
          glyphMarginHoverMessage: { value: 'Error' }
        }
      }]);
      
      this.editor.revealLineInCenter(line);
    }
    
    if (this.onError) {
      this.onError(formattedMessage);
    }
  }

  private clearError(): void {
    if (!this.editor) return;
    
    this.errorDecorations = this.editor.deltaDecorations(this.errorDecorations, []);
    
    if (this.onError) {
      this.onError(null); 
    }
  }

  /**
   * Get the current editor content
   */
  getValue(): string {
    return this.editor?.getValue() || '';
  }

  /**
   * Set the editor content with content tracking
   */
  public setValue(value: string, triggerAutoCheck: boolean = false, source: string = 'api'): void {
    if (!this.editor) return;
    
    // Temporarily disable auto-check during setValue to prevent unwanted checks
    const prevAutoCheck = this.autoCheckEnabled;
    this.autoCheckEnabled = triggerAutoCheck;
    
    // Set the value
    this.editor.setValue(value);
    
    // Restore previous auto-check setting
    this.autoCheckEnabled = prevAutoCheck;
    
    // Dispatch content change event to notify the app
    const hasContent = (value || '').trim() !== '';
    const event = new CustomEvent('editor-content-changed', {
      detail: { 
        hasContent, 
        changeSource: source,
        contentPreview: value.substring(0, 50) + (value.length > 50 ? '...' : '')
      }
    });
    document.dispatchEvent(event);
  }

  /**
   * Get type information at the current cursor position
   */
  async getTypeAtCursor(): Promise<string> {
    if (!this.editor) return 'Editor not initialized';
    
    const position = this.editor.getPosition();
    if (!position) return 'No cursor position';
    
    this.showLoading(true);
    try {
      const type = await prooflineApi.getTypeAtPosition(
        this.editor.getValue(),
        position.lineNumber,
        position.column
      );
      return type;
    } catch (error) {
      console.error('Get type at cursor failed:', error);
      return `Error: ${error}`;
    } finally {
      this.showLoading(false);
    }
  }

  /**
   * Set cursor position
   * @param line Line number
   * @param column Column number
   */
  setCursorPosition(line: number, column: number): void {
    if (!this.editor) return;
    
    this.editor.setPosition({ lineNumber: line, column: column });
    this.editor.revealPositionInCenter({ lineNumber: line, column: column });
    this.editor.focus();
  }

  /**
   * Focus the editor
   */
  focus(): void {
    this.editor?.focus();
  }

  /**
   * Dispose the editor
   */
  dispose(): void {
    this.editor?.dispose();
  }
}