import axios, { AxiosInstance } from 'axios';
import { 
  CheckResponse, 
  ProofState,
  Hole
} from '../models/ProofState';
import { extractHoleContexts } from '../utils/parserUtils';

export class ProoflineApi {
  private api: AxiosInstance;
  
  constructor(baseURL: string = 'http://localhost:3001') {
    this.api = axios.create({
      baseURL,
      timeout: 10000,
      headers: {
        'Content-Type': 'application/json'
      }
    });
  }

  /**
   * Update the API base URL
   * @param port The port number to use
   */
  updateBaseURL(port: string): void {
    this.api.defaults.baseURL = `http://localhost:${port}`;
  }

  /**
   * Test the connection to the API
   */
  async testConnection(): Promise<boolean> {
    try {
      const response = await this.api.get('/api/test');
      return response.status === 200;
    } catch (error) {
      console.error('Connection test failed:', error);
      return false;
    }
  }
  
  /**
   * Check a proof script
   * @param script The proof script to check
   */
  async checkProof(script: string): Promise<CheckResponse> {
    try {
      const response = await this.api.post('/api/check', {
        checkScript: script
      });
      
      if (response.data.success && response.data.state) {
        // Process and normalize the hole data, passing the term string for context extraction
        response.data.state.holes = this.normalizeHoles(
          response.data.state.holes, 
          response.data.term
        );
      }
      
      return response.data;
    } catch (error) {
      console.error('Check proof failed:', error);
      return {
        success: false,
        message: `Request failed: ${error}`
      };
    }
  }

  async loadFile(fileName: string): Promise<CheckResponse> {
    try {
      const response = await this.api.get(`/api/load?fileName=${encodeURIComponent(fileName)}`);
      
      if (response.data.success && response.data.state) {
        // Process and normalize the hole data with context extraction
        response.data.state.holes = this.normalizeHoles(
          response.data.state.holes,
          response.data.term
        );
      }
      
      return response.data;
    } catch (error) {
      console.error('Load file failed:', error);
      return {
        success: false,
        message: `Request failed: ${error}`
      };
    }
  }

  /**
   * Save a proof script to a file
   * @param fileName The name of the file to save to
   * @param content The content to save
   */
  async saveFile(fileName: string, content: string): Promise<boolean> {
    try {
      const response = await this.api.post('/api/save', {
        saveFileName: fileName,
        saveContent: content
      });
      return response.data.success;
    } catch (error) {
      console.error('Save file failed:', error);
      return false;
    }
  }

  /**
   * Get type at position
   * @param script The current script
   * @param line The line number
   * @param column The column number
   */
  async getTypeAtPosition(script: string, line: number, column: number): Promise<string> {
    try {
      const response = await this.api.post('/api/type-at-position', {
        script,
        position: { line, column }
      });
      return response.data.type || 'Unknown type';
    } catch (error) {
      console.error('Get type at position failed:', error);
      return 'Error retrieving type';
    }
  }
  
  /**
   * Normalize and process hole data from API responses
   * @param holes The holes array from the API response
   * @param termString Optional term string to extract code context from
   * @returns Normalized holes with consistent structure
   */
  private normalizeHoles(holes: any[], termString?: string): Hole[] {
    if (!holes || !Array.isArray(holes)) {
      return [];
    }
    
    // Extract code contexts from term string if provided using the shared utility
    const holeContexts = termString ? extractHoleContexts(termString) : new Map<string, string>();
    
    return holes.map(hole => {
      // Identify the hole ID - could be 'id' or 'holeId'
      const holeId = hole.id !== undefined ? hole.id : 
                    (hole.holeId !== undefined ? hole.holeId : 0);
                    
      // Get string version of holeId for context lookup
      const holeIdStr = typeof holeId === 'number' ? holeId.toString() : holeId;
      
      // Determine if the hole is solved - could be 'isSolved' or 'solved'
      const isSolved = typeof hole.isSolved === 'boolean' ? hole.isSolved : 
                      (typeof hole.solved === 'boolean' ? hole.solved : false);
      
      // Create the normalized hole object
      const normalizedHole: Hole = {
        holeId,
        isSolved
      };
      
      // For solved holes, use the 'value' field
      if (isSolved) {
        normalizedHole.value = hole.value || '';
      } 
      // For unsolved holes, use 'expectedType' or fall back to 'value' field which may contain type info
      else {
        normalizedHole.expectedType = hole.expectedType || hole.type || hole.value || 'Unknown';
      }
      
      // Process context data - carefully handle all possible formats
      if (hole.context) {
        if (Array.isArray(hole.context)) {
          // Process each context item to ensure correct format
          normalizedHole.context = hole.context.map((item: any) => {
            // Already in ContextItem format
            if (typeof item === 'object' && item !== null && item.name && item.type) {
              return item;
            }
            
            // String format with "name : type" pattern
            if (typeof item === 'string') {
              const parts = item.split(' : ');
              if (parts.length >= 2) {
                const name = parts[0].trim();
                const type = parts.slice(1).join(' : ').trim();
                return {
                  name,
                  type
                };
              }
              // Keep as string if doesn't match expected format
              return item;
            }
            
            // Fallback - convert to string
            return String(item);
          });
        } else if (typeof hole.context === 'string') {
          // Single string context
          normalizedHole.context = [hole.context];
        } else {
          // Empty context as fallback
          normalizedHole.context = [];
        }
      } else {
        normalizedHole.context = [];
      }
      
      // Add location if it exists
      if (hole.location) {
        normalizedHole.location = hole.location;
      }
      
      // Transfer existing codeContext if it exists directly on the hole
      if (hole.codeContext) {
        normalizedHole.codeContext = hole.codeContext;
      }
      // Add code context from term string if available
      else if (holeContexts.has(holeIdStr)) {
        normalizedHole.codeContext = holeContexts.get(holeIdStr);
      }
      
      return normalizedHole;
    });
  }
}

// Create and export a default instance
const prooflineApi = new ProoflineApi();
export default prooflineApi;