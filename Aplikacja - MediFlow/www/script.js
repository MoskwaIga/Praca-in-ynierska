
function adjustBoxHeights() {
  const boxes = document.querySelectorAll('.small-box');
  let maxHeight = 0;
  
  // Znajdź najwyższy kafelek
  boxes.forEach(box => {
    box.style.height = 'auto'; // Usuń wszelkie wcześniejsze ustawienia wysokości
    const height = box.offsetHeight;
    if (height > maxHeight) {
      maxHeight = height;
    }
  });
  
  // Ustaw wysokość wszystkich kafelków na najwyższą wysokość
  boxes.forEach(box => {
    box.style.height = `${maxHeight}px`;
  });
}

// Wywołaj funkcję po pełnym załadowaniu DOM
document.addEventListener('DOMContentLoaded', adjustBoxHeights);

// Wywołaj funkcję po załadowaniu wszystkich zasobów
window.addEventListener('load', adjustBoxHeights);

// Wywołaj funkcję przy zmianie rozmiaru okna
window.addEventListener('resize', adjustBoxHeights);

// Opcjonalnie: monitorowanie dynamicznych zmian w DOM
const observer = new MutationObserver(adjustBoxHeights);
observer.observe(document.body, { childList: true, subtree: true });


