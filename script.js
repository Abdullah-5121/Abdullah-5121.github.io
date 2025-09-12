document.addEventListener('DOMContentLoaded', () => {
    // Scroll-based fade-in for sections and cards
    const elementsToAnimate = document.querySelectorAll('.animated-section, .case-study, .connect-box');

    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                entry.target.classList.add('fade-in');
                observer.unobserve(entry.target);
            }
        });
    }, {
        threshold: 0.3 // Trigger when 30% of the element is visible
    });

    elementsToAnimate.forEach(element => {
        observer.observe(element);
    });
});
