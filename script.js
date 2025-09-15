// Theme toggle
const toggleBtn = document.getElementById("theme-toggle");
const body = document.body;

toggleBtn.addEventListener("click", () => {
  body.classList.toggle("dark-mode");
  body.classList.toggle("light-mode");

  // Change icon
  if (body.classList.contains("light-mode")) {
    toggleBtn.innerHTML = '<i class="fas fa-sun"></i>';
  } else {
    toggleBtn.innerHTML = '<i class="fas fa-moon"></i>';
  }
});

// Initialize Particles
particlesJS("particles-js", {
  "particles": {
    "number": {"value": 120, "density": {"enable": true, "value_area": 800}},
    "color": {"value": "#4fc3f7"},
    "shape": {"type": "circle"},
    "opacity": {"value": 0.6, "random": true, "anim": {"enable": true, "speed": 0.5, "opacity_min": 0.2}},
    "size": {"value": 4, "random": true, "anim": {"enable": true, "speed": 2, "size_min": 1}},
    "line_linked": {"enable": true, "distance": 120, "color": "#4fc3f7", "opacity": 0.4, "width": 1},
    "move": {"enable": true, "speed": 2, "random": true, "out_mode": "out"}
  },
  "interactivity": {
    "detect_on": "canvas",
    "events": {"onhover": {"enable": true, "mode": "grab"}, "onclick": {"enable": true, "mode": "push"}},
    "modes": {"grab": {"distance": 140, "line_linked": {"opacity": 0.5}}}
  },
  "retina_detect": true
});
