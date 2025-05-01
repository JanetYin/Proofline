import { Editor } from './components/Editor';
import { GoalsPanel } from './components/GoalsPanel';
import { HolesPanel } from './components/HolesPanel';
import { ErrorPanel } from './components/ErrorPanel';
import prooflineApi from './api/prooflineApi';
import { Hole, ProofState } from './models/ProofState';
import { processApiResponse, createGoalFromHole } from './utils/apiUtils';

// Define the app interface for sidebar integration
interface ProoflineAppInterface {
  openFile: () => void;
  saveFile: () => void;
  downloadFile: () => void;
  updatePort: () => Promise<void>;
}

// Declare global extensions
declare global {
  interface Window {
    app?: ProoflineAppInterface;
  }
}

export class ProoflineApp implements ProoflineAppInterface {
  private editor: Editor | null = null;
  private goalsPanel: GoalsPanel | null = null;
  private holesPanel: HolesPanel | null = null;
  private errorPanel: ErrorPanel | null = null;
  private currentState: ProofState | null = null;
  private statusBar: HTMLElement | null = null;
  private loadingSpinner: HTMLElement | null = null;
  private fileInput: HTMLInputElement | null = null;
  private isSidebarCollapsed: boolean = true; // Track sidebar state
  private isCheckingProof: boolean = false;
  private checkTimeoutId: number | null = null;
  private hasUserContent: boolean = false;
  private currentContentHash: string = '';

  /**
   * Check if there's already content in the editor
   */
  private checkForExistingContent(): void {
    if (!this.editor) return;
    
    const currentContent = this.editor.getValue();
    if (currentContent && currentContent.trim() !== '') {
      this.hasUserContent = true;
    }
  }

  /**
   * Generate a simple hash for content comparison
   */
  private hashContent(content: string): string {
    // Simple hash function for content comparison
    let hash = 0;
    if (content.length === 0) return hash.toString();
    
    for (let i = 0; i < content.length; i++) {
      const char = content.charCodeAt(i);
      hash = ((hash << 5) - hash) + char;
      hash = hash & hash; // Convert to 32bit integer
    }
    
    return hash.toString();
  }

  /**
   * Initialize the application
   */
  async init(): Promise<void> {
    // Set global reference for sidebar integration
    window.app = this;
    
    // Initialize status elements first for early feedback
    this.statusBar = document.getElementById('status-message');
    this.loadingSpinner = document.getElementById('loading-spinner');
    this.setStatus('Initializing Proofline...');

    try {
      // Initialize components
      this.editor = new Editor(
        'editor-container', 
        this.handleHoleSelect.bind(this),
        this.handleError.bind(this)
      );
      
      // Check if there's already content in the editor
      this.checkForExistingContent();
      
      // Initialize other UI components
      this.goalsPanel = new GoalsPanel('goals-container');
      this.holesPanel = new HolesPanel('holes-container', this.handleHoleSelect.bind(this));
      this.errorPanel = new ErrorPanel('error-container');
      
      // Create file input element for file selection
      this.createFileInput();
      
      // Initialize the VS Code-style sidebar
      this.initSidebar();
      
      // Setup event listeners
      this.setupEventListeners();

      // Try to connect to API server
      this.setStatus('Connecting to API server...');
      const isConnected = await prooflineApi.testConnection();
      this.updateConnectionStatus(isConnected);

      // Only load sample code if we don't have user content
      if (!this.hasUserContent) {
        this.loadSampleCodeWithoutCheck();
      } else {
        // If there's existing content, check it
        await this.checkProof();
      }
      
      this.setStatus('Ready');
    } catch (error) {
      console.error('Initialization error:', error);
      this.setStatus(`Initialization failed: ${error instanceof Error ? error.message : String(error)}`, true);
    }
  }

  /**
   * Initialize the sidebar with panel toggling
   */
  private initSidebar(): void {
    const sidebar = document.querySelector('.sidebar') as HTMLElement;
    const icons = ['file-icon', 'api-icon'];
    const panels = ['file-panel', 'api-panel'];
    
    // Set initial collapsed state
    if (this.isSidebarCollapsed && sidebar) {
      sidebar.classList.add('collapsed');
    }
    
    // Track active panel
    let activePanel: HTMLElement | null = null;
    
    // Setup toggle for each icon/panel pair
    icons.forEach((iconId, index) => {
      const icon = document.getElementById(iconId);
      const panel = document.getElementById(panels[index]);
      
      if (!icon || !panel || !sidebar) return;
      
      icon.addEventListener('click', () => {
        if (activePanel === panel) {
          // Close if already open
          panel.classList.remove('expanded');
          icon.classList.remove('active');
          activePanel = null;
          sidebar.classList.add('collapsed');
          this.isSidebarCollapsed = true;
        } else {
          // Expand sidebar if collapsed
          if (this.isSidebarCollapsed) {
            sidebar.classList.remove('collapsed');
            this.isSidebarCollapsed = false;
          }
          
          // Close any open panel
          icons.forEach(id => document.getElementById(id)?.classList.remove('active'));
          panels.forEach(id => document.getElementById(id)?.classList.remove('expanded'));
          
          // Open this panel
          panel.classList.add('expanded');
          icon.classList.add('active');
          activePanel = panel;
        }
        
        this.adjustEditorSize();
      });
    });
    
    // Close panel when clicking outside
    document.addEventListener('click', (event) => {
      if (!activePanel || !sidebar) return;
      
      if (event.target instanceof Node && !sidebar.contains(event.target)) {
        // Close active panel
        activePanel.classList.remove('expanded');
        icons.forEach(id => document.getElementById(id)?.classList.remove('active'));
        activePanel = null;
        
        sidebar.classList.add('collapsed');
        this.isSidebarCollapsed = true;
        
        this.adjustEditorSize();
      }
    }, true);
    
    // Setup file operation buttons
    this.setupFileOperationButtons();
    
    // Setup API port update
    this.setupApiPortUpdate();
    
    // Initial editor size adjustment
    this.adjustEditorSize();
  }

  /**
   * Setup file operation buttons
   */
  private setupFileOperationButtons(): void {
    const buttons = {
      open: document.getElementById('open-file-btn'),
      save: document.getElementById('save-file-btn'),
      download: document.getElementById('download-file-btn')
    };

    if (buttons.open) {
      buttons.open.addEventListener('click', () => {
        this.openFile();
        this.closeSidebarPanel('file-panel', 'file-icon');
      });
    }
    
    if (buttons.save) {
      buttons.save.addEventListener('click', () => {
        this.saveFile();
        this.closeSidebarPanel('file-panel', 'file-icon');
      });
    }
    
    if (buttons.download) {
      buttons.download.addEventListener('click', () => {
        this.downloadFile();
        this.closeSidebarPanel('file-panel', 'file-icon');
      });
    }
  }

  /**
   * Setup API port update button
   */
  private setupApiPortUpdate(): void {
    const updatePortBtn = document.getElementById('update-port-btn');
    if (updatePortBtn) {
      updatePortBtn.addEventListener('click', async () => {
        await this.updatePort();
        this.closeSidebarPanel('api-panel', 'api-icon');
      });
    }
  }

  /**
   * Close a sidebar panel
   */
  private closeSidebarPanel(panelId: string, iconId: string): void {
    const panel = document.getElementById(panelId);
    const icon = document.getElementById(iconId);
    const sidebar = document.querySelector('.sidebar') as HTMLElement;
    
    if (panel) panel.classList.remove('expanded');
    if (icon) icon.classList.remove('active');
    
    if (sidebar) {
      sidebar.classList.add('collapsed');
      this.isSidebarCollapsed = true;
    }
    
    this.adjustEditorSize();
  }
  
  /**
   * Adjust the editor size based on sidebar state
   */
  private adjustEditorSize(): void {
    if (this.editor) {
      setTimeout(() => {
        // This triggers Monaco editor to update its layout
        window.dispatchEvent(new Event('resize'));
      }, 300); // Small delay to allow CSS transitions to complete
    }
  }

  /**
   * Handle errors from the editor
   */
  private handleError(error: string | null): void {
    if (error && this.errorPanel) {
      this.errorPanel.show(error);
    } else if (this.errorPanel) {
      this.errorPanel.hide();
    }
  }

    /**
   * Create file input element for file selection
   */
  private createFileInput(): void {
    this.fileInput = document.createElement('input');
    this.fileInput.type = 'file';
    this.fileInput.accept = '.pl, .pf, .txt, .proof';  // Add .pl as the first option
    this.fileInput.style.display = 'none';
    document.body.appendChild(this.fileInput);
    
    // Add event listener for file selection
    if (this.fileInput) {
      this.fileInput.addEventListener('change', this.handleFileSelection.bind(this));
    }
  }

  /**
   * Public method to open a file (called from sidebar)
   */
  public openFile(): void {
    if (this.fileInput) {
      this.fileInput.click();
    }
  }

  /**
   * Public method to save a file (called from sidebar)
   */
  public saveFile(): void {
    this.saveToFile();
  }

  /**
   * Public method to download a file (called from sidebar)
   */
  public downloadFile(): void {
    this.downloadCurrentFile();
  }

  /**
   * Handle file selection from computer
   */
  private handleFileSelection(event: Event): void {
    const fileInput = event.target as HTMLInputElement;
    const file = fileInput.files?.[0];
    
    if (!file) {
      this.setStatus('No file selected', true);
      return;
    }
    
    this.setLoading(true);
    
    const reader = new FileReader();
    reader.onload = async (e) => {
      try {
        const fileContent = e.target?.result as string;
        
        if (this.editor) {
          // Set the file content with 'file-load' source
          this.editor.setValue(fileContent, false, 'file-load');
          
          // Mark that we have user content
          this.hasUserContent = true;
          
          // Update content hash
          this.currentContentHash = this.hashContent(fileContent);
          
          // Automatically check the proof after loading
          await this.checkProof();
        }
        
        this.setStatus(`File "${file.name}" loaded successfully`);
      } catch (error) {
        console.error('Error reading file:', error);
        this.setStatus(`Failed to read file: ${error instanceof Error ? error.message : String(error)}`, true);
      } finally {
        this.setLoading(false);
        
        // Reset file input
        if (this.fileInput) {
          this.fileInput.value = '';
        }
      }
    };
    
    reader.onerror = () => {
      this.setStatus('Error reading file', true);
      this.setLoading(false);
    };
    
    reader.readAsText(file);
  }

  /**
   * Setup event listeners for the application
   */
  private setupEventListeners(): void {
    // Button event listeners
    const checkProofBtn = document.getElementById('check-proof-btn');
    if (checkProofBtn) {
      checkProofBtn.addEventListener('click', () => {
        // Force immediate check of the current content without debounce
        if (this.checkTimeoutId !== null) {
          window.clearTimeout(this.checkTimeoutId);
          this.checkTimeoutId = null;
        }
        
        // Execute check immediately with current editor content
        this.executeProofCheck();
      });
    }

    // Listen for all editor content changes
    document.addEventListener('editor-content-changed', ((e: CustomEvent) => {
      const detail = e.detail;
      
      // If we have content, mark it as user content
      if (detail.hasContent) {
        this.hasUserContent = true;
        
        // Get the full content and update hash
        if (this.editor) {
          const currentContent = this.editor.getValue();
          const newHash = this.hashContent(currentContent);
          
          // Only update if content actually changed (prevents loops)
          if (newHash !== this.currentContentHash) {
            this.currentContentHash = newHash;
          }
        }
      }
    }) as EventListener);

    // Set up goal-related events
    this.setupGoalRelatedEvents();
  }

  /**
   * Handle hole selection
   * @param hole The selected hole
   */
  private handleHoleSelect(hole: Hole): void {
    if (!this.currentState || !this.goalsPanel) return;
    
    // Ensure hole has all required properties
    const validHole: Hole = {
      ...hole,
      context: hole.context || []
    };

    // Get the current script
    const currentScript = this.editor ? this.editor.getValue() : this.currentState.script;
    
    // Create a goal object from the hole for display, passing the script for context
    const currentGoal = createGoalFromHole(validHole, currentScript);
    
    if (currentGoal) {
      // Update goals panel with both the goal and the original hole
      this.goalsPanel.updateGoal(currentGoal, validHole);
      
      // Scroll to the hole in the editor but don't highlight it
      if (this.editor && hole.location) {
        this.editor.setCursorPosition(hole.location.startLine, hole.location.startColumn);
      }
    } else {
      this.goalsPanel.clear();
    }
  }

  /**
   * Check if all holes are solved and return result
   */
  private areAllHolesSolved(): boolean {
    if (!this.currentState?.holes || this.currentState.holes.length === 0) {
      return false;
    }
    
    // Check if any hole is unsolved
    return !this.currentState.holes.some(hole => !hole.isSolved);
  }
  
  /**
   * Set up event listeners for goal-related events
   */
  private setupGoalRelatedEvents(): void {
    // Listen for "check-all-holes-solved" event
    document.addEventListener('check-all-holes-solved', ((e: CustomEvent) => {
      const callback = e.detail?.callback;
      if (typeof callback === 'function') {
        const allSolved = this.areAllHolesSolved();
        callback(allSolved);
      }
    }) as EventListener);
  }

  /**
   * Check the current proof
   */
  private async checkProof(): Promise<void> {
    // Prevent multiple simultaneous checks
    if (this.isCheckingProof) {
      return;
    }

    // Clear any pending timeouts
    if (this.checkTimeoutId !== null) {
      window.clearTimeout(this.checkTimeoutId);
      this.checkTimeoutId = null;
    }

    // Set a timeout before checking to debounce rapid checks
    this.checkTimeoutId = window.setTimeout(async () => {
      await this.executeProofCheck();
      this.checkTimeoutId = null;
    }, 100);
  }
  
  /**
   * Execute the actual proof check
   */
  private async executeProofCheck(): Promise<void> {
    if (!this.editor) {
      console.error("Cannot check proof: Editor not initialized");
      return;
    }

    // Get the current code directly from the editor
    const code = this.editor.getValue();
    
    if (!code.trim()) {
      this.setStatus('Please enter code', true);
      return;
    }

    // Set flag to prevent multiple checks
    this.isCheckingProof = true;
    
    this.setLoading(true);
    try {
      const response = await prooflineApi.checkProof(code);
      
      if (response.success) {
        // Process the API response
        const processedState = processApiResponse(response, code);
        
        if (processedState) {
          // Update the current state
          this.currentState = processedState;
          
          // Ensure the script in the state matches what's in the editor
          this.currentState.script = code;
          
          // Update holes panel with the new holes
          if (this.holesPanel) {
            this.holesPanel.updateHoles([...this.currentState.holes]);
          }
          
          // Clear any previous errors
          if (this.errorPanel) {
            this.errorPanel.hide();
          }

          this.setStatus('Proof check successful');
        } else {
          console.error("Failed to process API response");
          this.setStatus('Error processing API response', true);
        }
      } else {
        if (this.errorPanel && response.message) {
          this.errorPanel.show(response.message);
        }
        this.setStatus(`Proof check failed`, true);
      }
    } catch (error) {
      console.error('Error checking proof:', error);
      this.setStatus(`Request failed: ${error instanceof Error ? error.message : String(error)}`, true);
    } finally {
      this.setLoading(false);
      // Reset flag to allow new checks
      this.isCheckingProof = false;
    }
  }

    /**
   * Save the current proof to a file on the server
   */
  private async saveToFile(): Promise<void> {
    if (!this.editor) return;

    const code = this.editor.getValue();
    // Prompt for filename with .pl extension as default
    const fileName = prompt('Enter filename to save:', 'proof.pl');

    if (!fileName) {
      this.setStatus('Save cancelled', false);
      return;
    }

    if (!code.trim()) {
      this.setStatus('Nothing to save, editor is empty', true);
      return;
    }

    this.setLoading(true);
    try {
      const success = await prooflineApi.saveFile(fileName, code);

      if (success) {
        this.setStatus(`File "${fileName}" saved successfully`);
      } else {
        this.setStatus(`Save failed`, true);
      }
    } catch (error) {
      console.error('Error saving file:', error);
      this.setStatus(`Request failed: ${error instanceof Error ? error.message : String(error)}`, true);
    } finally {
      this.setLoading(false);
    }
  }

  /**
   * Download the current proof to a file on the user's computer
   */
  private downloadCurrentFile(): void {
    if (!this.editor) return;
    
    // Get file content
    const code = this.editor.getValue();
    
    // Prompt for filename with .pl extension as default
    const fileName = prompt('Enter filename to download:', 'proof.pl');
    
    if (!fileName) {
      this.setStatus('Download cancelled', false);
      return;
    }
    
    if (!code.trim()) {
      this.setStatus('Nothing to download, editor is empty', true);
      return;
    }
    
    // Create a blob with the content
    const blob = new Blob([code], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    
    // Create a temporary download link
    const downloadLink = document.createElement('a');
    downloadLink.href = url;
    downloadLink.download = fileName;
    
    // Append to the document, click it, and remove it
    document.body.appendChild(downloadLink);
    downloadLink.click();
    document.body.removeChild(downloadLink);
    
    // Clean up blob URL
    URL.revokeObjectURL(url);
    
    this.setStatus(`File "${fileName}" downloaded successfully`);
  }
  /**
   * Public method to update the API port and test connection
   */
  public async updatePort(): Promise<void> {
    const portInput = document.getElementById('port-input') as HTMLInputElement;
    if (!portInput) return;

    const port = portInput.value;
    prooflineApi.updateBaseURL(port);
    
    this.setStatus(`Port updated to: ${port}`);
    
    // Automatically test connection after updating port
    await this.testConnection();
  }

  /**
   * Test connection to the API server
   */
  private async testConnection(): Promise<void> {
    const connectionStatus = document.getElementById('connection-status');
    if (connectionStatus) {
      connectionStatus.textContent = 'Connecting...';
      connectionStatus.className = 'status-indicator';
    }

    this.setLoading(true);
    try {
      const isConnected = await prooflineApi.testConnection();
      this.updateConnectionStatus(isConnected);
    } catch (error) {
      console.error('Error testing connection:', error);
      this.updateConnectionStatus(false);
      this.setStatus(`Connection failed: ${error instanceof Error ? error.message : String(error)}`, true);
    } finally {
      this.setLoading(false);
    }
  }

  /**
   * Update the connection status UI
   * @param isConnected Whether the connection is successful
   */
  private updateConnectionStatus(isConnected: boolean): void {
    const connectionStatus = document.getElementById('connection-status');
    if (!connectionStatus) return;

    if (isConnected) {
      connectionStatus.textContent = 'Connected';
      connectionStatus.className = 'status-indicator connected';
      this.setStatus('Successfully connected to API server');
    } else {
      connectionStatus.textContent = 'Disconnected';
      connectionStatus.className = 'status-indicator disconnected';
      this.setStatus('Failed to connect to API server', true);
    }
  }

  /**
   * Set the status message
   * @param message The status message
   * @param isError Whether the message is an error
   */
  private setStatus(message: string, isError: boolean = false): void {
    if (!this.statusBar) return;

    this.statusBar.textContent = message;
    this.statusBar.style.color = isError ? 'var(--danger-color)' : 'var(--text-color)';

    // Auto-clear non-error status messages
    if (!isError) {
      setTimeout(() => {
        if (this.statusBar) {
          this.statusBar.textContent = 'Ready';
          this.statusBar.style.color = 'var(--text-color)';
        }
      }, 5000);
    }
  }

  /**
   * Set the loading state
   * @param isLoading Whether loading is in progress
   */
  private setLoading(isLoading: boolean): void {
    if (!this.loadingSpinner) return;

    this.loadingSpinner.style.display = isLoading ? 'inline-block' : 'none';

    // Disable buttons during loading
    const buttons = document.querySelectorAll('button');
    buttons.forEach(button => {
      button.disabled = isLoading;
    });
  }

  /**
   * Load sample code into the editor without checking
   */
  private loadSampleCodeWithoutCheck(): void {
    if (!this.editor) {
      console.error("Cannot load sample code: Editor not initialized");
      return;
    }

    const sampleCode = `let id : (A : _) -> A -> A = \\A x. x;
let List : U -> U = \\A. (L : _) -> (A -> L -> L) -> L -> L;
let nil : (A : _) -> List A = \\A L cons nil. nil;
let cons : (A : _) -> A -> List A -> List A = \\A x xs L cons nil. cons x (xs _ cons nil);
let Bool : U = (B : _) -> B -> B -> B;
let true : Bool = \\B t f. t;
let false : Bool = \\B t f. f;
let not : Bool -> Bool = \\b B t f. b B f t;
let list1 : List Bool = cons _ _ (nil _);
U
`;

    // Set the editor content but explicitly skip automatic check
    this.editor.setValue(sampleCode);
  }
}