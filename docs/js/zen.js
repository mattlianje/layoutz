/* Zen — parallax-lite hero effect */
document.addEventListener("DOMContentLoaded", () => {
  const hero = document.querySelector(".hero");
  if (hero) {
    window.addEventListener("scroll", () => {
      const y = window.scrollY;
      if (y < 800) {
        hero.style.transform = "translateY(" + (y * 0.08) + "px)";
      }
    }, { passive: true });
  }
});
