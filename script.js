document.addEventListener('DOMContentLoaded', () => {
    // Select all elements you want to animate on scroll
    const elementsToAnimate = document.querySelectorAll('header, section, .connect-box');

    // Create a new Intersection Observer
    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                // When the element is visible, add the 'fade-in' class
                entry.target.classList.add('fade-in');
                // Stop observing the element once it has animated
                observer.unobserve(entry.target);
            }
        });
    }, {
        // Options for the observer
        threshold: 0.3 // Trigger when 30% of the element is visible
    });

    // Tell the observer to watch all the selected elements
    elementsToAnimate.forEach(element => {
        observer.observe(element);
    });
});
