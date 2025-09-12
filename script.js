document.addEventListener('DOMContentLoaded', () => {
    // Scroll-based fade-in for sections
    const sections = document.querySelectorAll('.animated-section');

    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                entry.target.classList.add('fade-in');
                observer.unobserve(entry.target);
            }
        });
    }, {
        threshold: 0.3 // Trigger when 30% of the section is visible
    });

    sections.forEach(section => {
        observer.observe(section);
    });
});
