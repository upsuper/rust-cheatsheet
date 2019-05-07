const root = document.documentElement;
const args = location.search.slice(1).split(',').filter(arg => !!arg);
for (const arg of args) {
  switch (arg) {
    case 'dark':
      document.getElementById('theme').href = 'theme-dark.css';
      break;
    case 'large':
    case 'single':
      root.classList.add(arg);
      break;
    default:
      console.warn(`Unknown argument ${arg}`);
  }
}

window.addEventListener("DOMContentLoaded", () => {
  const footer = document.querySelector('footer');
  const modeSwitches = footer.querySelectorAll('li > a[href^="?"]');
  for (const a of modeSwitches) {
    const mode = a.getAttribute('href').slice(1);
    if (args.includes(mode)) {
      a.parentNode.classList.add('on');
      a.href = '?' + args.filter(arg => arg !== mode).join(',');
    } else {
      a.href = '?' + [...args, mode].join(',');
    }
  }
}, { once: true });
