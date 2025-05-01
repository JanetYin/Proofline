import { Hole } from '../models/ProofState';

export class HolesPanel {
  private container: HTMLElement;
  private holes: Hole[] = [];
  private onHoleSelect: (hole: Hole) => void;

  constructor(containerId: string, onHoleSelect: (hole: Hole) => void) {
    const element = document.getElementById(containerId);
    if (!element) {
      throw new Error(`Container element with ID ${containerId} not found`);
    }
    this.container = element;
    this.onHoleSelect = onHoleSelect;
  }

  /**
   * Update the holes panel with new holes
   * @param holes The holes to display
   */
  updateHoles(holes: Hole[]): void {
    // Deep copy the holes array to avoid reference issues
    this.holes = holes.map(hole => ({
      ...hole,
      context: hole.context ? [...hole.context] : [] 
    }));
    
    // Force a complete re-render of the panel
    this.clear();
    this.render();
  }

  /**
   * Clear the holes panel
   */
  clear(): void {
    this.container.innerHTML = '';
  }

  /**
   * Render the holes panel
   */
  private render(): void {
    if (!this.holes || this.holes.length === 0) {
      this.renderEmptyState();
      return;
    }

    // Create holes list
    const holesList = document.createElement('div');
    holesList.className = 'holes-list';

    // Sort holes: unsolved first, then by ID
    const sortedHoles = [...this.holes].sort((a, b) => {
      // First sort by solved status (unsolved first)
      if (a.isSolved !== b.isSolved) {
        return a.isSolved ? 1 : -1;
      }
      // Then sort by ID
      const idA = typeof a.holeId === 'number' ? a.holeId : parseInt(String(a.holeId), 10);
      const idB = typeof b.holeId === 'number' ? b.holeId : parseInt(String(b.holeId), 10);
      return idA - idB;
    });

    sortedHoles.forEach(hole => {
      const holeItem = document.createElement('div');
      holeItem.className = `hole-item ${hole.isSolved ? 'solved' : 'unsolved'}`;
      holeItem.setAttribute('data-hole-id', String(hole.holeId));

      // Create hole header with ID and status
      const holeHeader = document.createElement('div');
      holeHeader.className = 'hole-header';
      
      const holeId = document.createElement('span');
      holeId.className = 'hole-id';
      holeId.textContent = `Hole #${hole.holeId}`;
      
      const holeStatus = document.createElement('span');
      holeStatus.className = 'hole-status';
      holeStatus.textContent = hole.isSolved ? 'Solved' : 'Unsolved';
      
      holeHeader.appendChild(holeId);
      holeHeader.appendChild(holeStatus);
      holeItem.appendChild(holeHeader);

      // Create hole details
      const holeDetails = document.createElement('div');
      holeDetails.className = 'hole-details';
      
      // Display the type information
      const holeType = document.createElement('div');
      holeType.className = 'hole-type';
      
      // Different display for solved and unsolved holes
      if (hole.isSolved) {
        // For solved holes, show the value
        holeType.innerHTML = `<strong>Value:</strong> ${this.escapeHtml(hole.value || '')}`;
      } else {
        // For unsolved holes, show the expected type
        holeType.innerHTML = `<strong>Type:</strong> ${this.escapeHtml(hole.expectedType || 'Unknown')}`;
      }
      
      holeDetails.appendChild(holeType);
      
      // Add context preview indicator if context exists
      if (!hole.isSolved && hole.context && hole.context.length > 0) {
        const contextPreview = document.createElement('div');
        contextPreview.className = 'context-preview';
        contextPreview.innerHTML = `<strong>Context:</strong> ${hole.context.length} items`;
        holeDetails.appendChild(contextPreview);
      }
      
      holeItem.appendChild(holeDetails);

      // Add event listener for hole selection
      holeItem.style.cursor = 'pointer';
      holeItem.addEventListener('click', () => {
        // Highlight the selected hole
        const allHoleItems = this.container.querySelectorAll('.hole-item');
        allHoleItems.forEach(item => item.classList.remove('selected'));
        holeItem.classList.add('selected');
        
        // Ensure context field exists
        const validHole = {
          ...hole,
          context: Array.isArray(hole.context) ? hole.context : []
        };
        
        this.onHoleSelect(validHole);
      });

      holesList.appendChild(holeItem);
    });

    this.container.appendChild(holesList);
  }

  /**
   * Render empty state
   */
  private renderEmptyState(): void {
    const emptyState = document.createElement('div');
    emptyState.className = 'empty-holes-state';
    emptyState.innerHTML = `
      <p>No holes detected</p>
      <p>Your proof might be complete, or you need to check it first</p>
    `;
    this.container.appendChild(emptyState);
  }

  /**
   * Escape HTML to prevent XSS
   * @param html The HTML string to escape
   */
  private escapeHtml(html: string): string {
    if (html === undefined || html === null) {
      return '';
    }
    const div = document.createElement('div');
    div.textContent = String(html);
    return div.innerHTML;
  }
}