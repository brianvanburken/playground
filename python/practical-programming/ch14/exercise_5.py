from math import sqrt


class Point:
    def __init__(self, x: int, y: int) -> None:
        """ Creates a new point

        >>> point = Point(1, 2)
        >>> point.x
        1
        >>> point.y
        2
        """
        self.x = x
        self.y = y

    def __repr__(self) -> str:
        """ Returns a string representation of the point.

        >>> point = Point(1, 2)
        >>> point
        Point(1, 2)
        """
        return 'Point({}, {})'.format(self.x, self.y)


class LineSegment:
    def __init__(self, point1: Point, point2: Point) -> None:
        """ Creates a line segment

        >>> point1 = Point(1, 1)
        >>> point2 = Point(3, 2)
        >>> segment = LineSegment(point1, point2)
        >>> segment.point1
        Point(1, 1)
        >>> segment.point2
        Point(3, 2)
        """
        self.point1 = point1
        self.point2 = point2

    def slope(self) -> float:
        """ Returns the slope of the segment.

        >>> segment = LineSegment(Point(1, 1), Point(3, 2))
        >>> segment.slope()
        0.5
        """
        return (abs(self.point2.y - self.point1.y) /
                abs(self.point2.x - self.point1.x))

    def length(self) -> float:
        """ returns the length of the segment.

        >>> segment = LineSegment(Point(1, 1), Point(3, 2))
        >>> segment.length()
        2.23606797749979
        """
        a = (self.point2.x - self.point1.x)
        b = (self.point2.y - self.point1.y)
        return sqrt(a * a + b * b)

if __name__ == "__main__":
    import doctest
    doctest.testmod()
