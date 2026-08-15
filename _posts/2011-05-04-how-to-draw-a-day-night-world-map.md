---
layout: post
title: How to Draw a Day/Night World Map
date: 2011-05-04 14:05:00.011000+00:00
---

![](/assets/images/posts/how-to-draw-a-day-night-world-map/img-01.png)

Actually, there is nothing too hard, especially if you have a good astronomical algorithm library. Nowadays we could do this using only plain JavaScript and HTML5 canvas.

![](/assets/images/posts/how-to-draw-a-day-night-world-map/img-02.png)
Display the current sunlight map¹
  
  
¹ Make sure that *daylight saving time* settings are configured properly on your system

 The algorithm is the following:

1. To obtain the latitude and longitude of the [subsolar point](http://en.wikipedia.org/wiki/Subsolar_point) find the current [equatorial coordinates](http://en.wikipedia.org/wiki/Equatorial_coordinate_system) of the Sun: the hour angle and declination.
2. Map each pixel of the Earth map to the corresponding [geographical coordinates](http://en.wikipedia.org/wiki/Geographic_coordinate) using its [map projection](http://en.wikipedia.org/wiki/Map_projection) transformations.
3. Map each geographical coordinate of a pixel to the corresponding equatorial coordinates.
4. Using equatorial coordinates check whether each pixel of the map is distant from the subsolar point farther than 90 degrees (because the shadow edge is a great circle).
5. If so, convert RGB values of the pixel to [HSV](http://en.wikipedia.org/wiki/HSL_and_HSV) color space and decrease the Value channel by two (or get the same pixel from a night version of the map), then put processed pixel data back using reverse HSV to RGB conversion.
6. That's all.

Implementation details:
  

1. To find estimate subsolar point coordinates it is only necessary to correct the current [UTC](http://en.wikipedia.org/wiki/Coordinated_Universal_Time) time with [the equation of time](http://en.wikipedia.org/wiki/Equation_of_time) (to get the hour angle) and calculate the position of the Sun on the [ecliptic](http://en.wikipedia.org/wiki/Ecliptic) using the amount of time since the vernal [equinox](http://en.wikipedia.org/wiki/Equinox) (to get the declination). [This](http://web.archive.org/web/20100825075745/http://www.usno.navy.mil/USNO/astronomical-applications/astronomical-information-center/approx-solar) approximate algorithm also works. But there are more precise algorithms that deal with Earth axial [precessions and nutations](http://en.wikipedia.org/wiki/Nutation), elliptical motion and other periodic theories described in the book *Astronomical Algorithms by Jean Meeus*. Because it is very hard to implement and debug them properly, we will use [existing](http://www.celnav.de/longterm.htm) JavaScript implementation by [Henning Umland](http://www.celnav.de/).
2. Since we use the [equirectangular](http://en.wikipedia.org/wiki/Equirectangular_projection)
   map projection, there is no need for Cartesian/geographical coordinate
   conversion - the longitude and latitude are mapped directly to the
   Cartesian coordinates. We only need to adjust the origin of the map.
3. To find the angular distance between the subsolar point and the pixel being processed we need to apply [the spherical law of cosines](http://en.wikipedia.org/wiki/Great-circle_distance) to spherical (equatorial) coordinates of the corresponding points obtained from the geographical ones.
4. Because HTML5 canvas has the ability to draw semitransparent rectangles, we do not need to mess with HSV color representation and can optimize the algorithm further. As we know that a projection of the sunlight shadow on the Earth map is a sine-like figure (sine is an unambiguous mapping), we only need to find the first top or bottom pixel (depends on the current season) of the shadow for a given pixel column and draw one pixel wide semitransparent rectangle to the opposite map edge.
5. See [Windows Desktop Gadget](https://gchristensen.github.io/#sunlight) on GitHub for an HTML5 demo.

See also:  [How to Design Software Systems?](https://gchristensen.github.io/posts/how-to-design-software-systems-qmark/)
