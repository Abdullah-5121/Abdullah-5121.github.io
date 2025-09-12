document.addEventListener('DOMContentLoaded', () => {
    // Scroll-based fade-in for sections
    const sections = document.querySelectorAll('.animated-section');

    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                entry.target.classList.add('fade-in');
            } else {
                // Optional: remove 'fade-in' when out of view to allow re-animation on scroll up
                // entry.target.classList.remove('fade-in'); 
            }
        });
    }, {
        threshold: 0.3 // Trigger when 30% of the section is visible
    });

    sections.forEach(section => {
        observer.observe(section);
    });

    // Dynamic background grid opacity based on scroll
    const backgroundGrid = document.querySelector('.background-grid');
    const totalHeight = document.body.scrollHeight - window.innerHeight; // Total scrollable height

    window.addEventListener('scroll', () => {
        const scrollPosition = window.scrollY;
        
        // Adjust opacity based on scroll position (example: fades out slightly as you scroll down)
        // You can make this more complex, e.g., change colors or add more elements
        let opacity = 0.1 + (scrollPosition / totalHeight) * 0.1; // Increases opacity slightly
        if (opacity > 0.2) opacity = 0.2; // Cap opacity
        backgroundGrid.style.opacity = opacity;
    });

    // Interactive Header Hover Effect (JavaScript for more complex effects)
    const interactiveHeader = document.querySelector('.interactive-header');
    if (interactiveHeader) {
        interactiveHeader.addEventListener('mousemove', (e) => {
            const rect = interactiveHeader.getBoundingClientRect();
            const x = e.clientX - rect.left; // x position within the element
            const y = e.clientY - rect.top;  // y position within the element

            // Simple parallax/tilt effect based on mouse position
            const xAxis = (rect.width / 2 - x) / 25; // Divide by a larger number for less tilt
            const yAxis = (rect.height / 2 - y) / 25;

            interactiveHeader.style.transform = `
                perspective(1000px)
                rotateX(${yAxis}deg)
                rotateY(${-xAxis}deg)
                scale(1.02)
            `;
            
            // Optional: Move glowing pseudo-element background slightly
            interactiveHeader.style.setProperty('--glow-x', `${x}px`);
            interactiveHeader.style.setProperty('--glow-y', `${y}px`);
        });

        interactiveHeader.addEventListener('mouseleave', () => {
            interactiveHeader.style.transform = `
                perspective(1000px)
                rotateX(0deg)
                rotateY(0deg)
                scale(1)
            `;
        });
    }
});
