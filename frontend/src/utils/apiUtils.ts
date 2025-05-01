import { ProofState, Hole, ContextItem, Goal } from '../models/ProofState';
import { findHoleLocations } from './holeUtils';
import { getHoleCodeContext, getHoleCodeRepresentation } from './parserUtils';

/**
 * Process the raw API response to ensure correct hole data format
 * @param response The API response data
 * @param currentScript The current script text (for finding hole locations)
 * @returns Processed proof state with properly formatted holes
 */
export function processApiResponse(response: any, currentScript: string): ProofState | null {
  if (!response || !response.success) {
    console.error('Invalid API response:', response);
    return null;
  }
  
  try {
    // Get state from response, fallback to creating a new one
    const state: ProofState = response.state || {
      script: currentScript,  // Default to current script if no state
      holes: [],
      fileName: 'current.proof'
    };
    
    // IMPORTANT: Always use the current script from the editor
    // This ensures we're working with what the user sees
    state.script = currentScript;
    
    // Get holes data from wherever it might be in the response
    const holesData = (response.state && response.state.holes) || response.holes || [];
    
    // Process each hole
    const processedHoles = holesData.map((hole: any): Hole => {
      // Convert string IDs to numbers if possible
      let holeId = hole.id || hole.holeId || 0;
      if (typeof holeId === 'string' && !isNaN(Number(holeId))) {
        holeId = Number(holeId);
      }
      
      // Base hole object with required fields
      const processedHole: Hole = {
        holeId,
        isSolved: typeof hole.isSolved === 'boolean' ? hole.isSolved : 
                  (typeof hole.solved === 'boolean' ? hole.solved : false)
      };
      
      // For solved holes, use the value
      if (processedHole.isSolved && hole.value) {
        processedHole.value = hole.value;
      }
      
      // Get the expected type from the correct field
      if (!processedHole.isSolved) {
        processedHole.expectedType = hole.expectedType || hole.type || hole.value || 'Unknown';
      }
      
      // Process context data
      if (hole.context) {
        if (Array.isArray(hole.context)) {
          // Convert context items to proper format
          processedHole.context = hole.context.map((item: any) => {
            // If item is already a proper context item object
            if (typeof item === 'object' && item !== null && item.name && item.type) {
              return item;
            }
            
            // If item is a string with format "name : type"
            if (typeof item === 'string') {
              const parts = item.split(' : ');
              if (parts.length >= 2) {
                return {
                  name: parts[0].trim(),
                  type: parts.slice(1).join(' : ').trim()
                } as ContextItem;
              }
              return item; // Keep as is if not in the expected format
            }
            
            // Fallback
            return String(item);
          });
        } else if (typeof hole.context === 'string') {
          // Single string context, wrap in array
          processedHole.context = [hole.context];
        } else {
          // Empty context
          processedHole.context = [];
        }
      } else {
        processedHole.context = [];
      }
      
      // Preserve the codeContext from the hole data if it exists
      if (hole.codeContext) {
        processedHole.codeContext = hole.codeContext;
      }
      
      return processedHole;
    });
    
    // Update state with processed holes
    state.holes = processedHoles;
    
    // Add location data by scanning the script
    if (currentScript) {
      state.holes = findHoleLocations(currentScript, state.holes);
    }
    
    return state;
  } catch (error) {
    console.error('Error processing API response:', error);
    return null;
  }
}

/**
 * Create a goal object from a hole for display in the goals panel
 * @param hole The selected hole
 * @param script Optional full script text for context
 * @returns Goal object for display
 */
export function createGoalFromHole(hole: Hole, script?: string): Goal | null {
  if (!hole) return null;
  
  const goal: Goal = {
    holeId: hole.holeId,
    type: hole.expectedType || 'Unknown type',
    context: hole.context || [],
    codeRepresentation: getHoleCodeRepresentation(hole)
  };
  
  // IMPORTANT: Always use the codeContext that comes from the hole
  // This ensures we use what the API provided directly
  if (hole.codeContext) {
    goal.codeContext = hole.codeContext;
  } 
  // Only generate code context if the hole doesn't already have it
  else if (script && hole.location) {
    goal.codeContext = getHoleCodeContext(script, hole, 2);
  }
  
  return goal;
}