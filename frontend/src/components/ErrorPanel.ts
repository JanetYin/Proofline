import { formatErrorAsHTML } from '../utils/parserUtils';

export class ErrorPanel {
  private container: HTMLElement;
  
  constructor(containerId: string) {
    this.container = document.getElementById(containerId) || document.createElement('div');
    this.hide();
  }
  
  show(message: string): void {
    // Format the error message using the shared utility
    this.container.innerHTML = formatErrorAsHTML(message);
    this.container.style.display = 'block';
  }
  
  hide(): void {
    this.container.style.display = 'none';
  }
}