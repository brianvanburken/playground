class Member:
    """ A member of a university. """

    def __init__(self, name: str, address: str, email: str) -> None:
        """Create a new member named name, with home address and email address.
        """
        self.name = name
        self.address = address
        self.email = email

    def __str__(self) -> str:
        """Return a string representation of this Member.

        >>> member = Member('Paul', 'Ajax', 'pgries@cs.toronto.edu')
        >>> member.__str__()
        'Paul\\nAjax\\npgries@cs.toronto.edu'
        """
        return '{}\n{}\n{}'.format(self.name, self.address, self.email)

    def __repr__(self) -> str:
        """Return a string representation of this Member.

        >>> member = Member('Paul', 'Ajax', 'pgries@cs.toronto.edu')
        >>> member
        Member('Paul', 'Ajax', 'pgries@cs.toronto.edu')
        """
        return "Member('{0}', '{1}', '{2}')".format(
            self.name, self.address, self.email)



class Faculty(Member):
    """ A faculty member at a university. """

    def __init__(self, name: str, address: str, email: str,
                 faculty_num: str) -> None:
        """Create a new faculty named name, with home address, email address,
        faculty number faculty_num, and empty list of courses.
        """

        super().__init__(name, address, email)
        self.faculty_number = faculty_num
        self.courses_teaching = []

    def __str__(self) -> str:
        """Return a string representation of this Faculty.

        >>> faculty = Faculty('Paul', 'Ajax', 'pgries@cs.toronto.edu', '1234')
        >>> faculty.__str__()
        'Paul\\nAjax\\npgries@cs.toronto.edu\\n1234\\nCourses: '
        """
        member_string = super().__str__()
        return '''{}\n{}\nCourses: {}'''.format(
            member_string,
            self.faculty_number,
            ' '.join(self.courses_teaching))

    def __repr__(self) -> str:
        """Return a string representation of this Faculty.

        >>> faculty = Faculty('Paul', 'Ajax', 'pgries@cs.toronto.edu', '1234')
        >>> faculty
        Faculty('Paul', 'Ajax', 'pgries@cs.toronto.edu', '1234')
        """
        return "Faculty('{0}', '{1}', '{2}', '{3}')".format(
            self.name, self.address, self.email, self.faculty_number)


class Student(Member):
    """ A student member at a university. """

    def __init__(self, name: str, address: str, email: str,
                 student_num: str) -> None:
        """ Create a new student named name, with home address, email address,
        student number student_num, an empty list of courses taken, and an
        empty list of current courses.

        >>> student = Student('Paul', 'Ajax', 'pgries@cs.toronto.edu', '120')
        >>> student.name
        'Paul'
        >>> student.address
        'Ajax'
        >>> student.email
        'pgries@cs.toronto.edu'
        >>> student.student_number
        '120'
        """
        super().__init__(name, address, email)
        self.student_number = student_num
        self.courses_taken = []
        self.courses_taking = []

    def __str__(self) -> str:
        """ Return a string representation of the student.

        >>> student = Student('Paul', 'Ajax', 'pgries@cs.toronto.edu', '120')
        >>> student.__str__()
        'Paul\\nAjax\\npgries@cs.toronto.edu\\nStudent Number: 120\\nCourses Taken: \\nCourses Taking: '
        """
        member_string = super().__str__()
        return '''{0}\nStudent Number: {1}\nCourses Taken: {2}\nCourses Taking: {3}''' \
            .format(
                member_string,
                self.student_number,
                ' '.join(self.courses_taken),
                ' '.join(self.courses_taking)
            )

    def __repr__(self):
        """ Return a string representation of the student.

        >>> student = Student('Paul', 'Ajax', 'pgries@cs.toronto.edu', '120')
        >>> student
        Student('Paul', 'Ajax', 'pgries@cs.toronto.edu', '120')
        """
        return "Student('{0}', '{1}', '{2}', '{3}')".format(
            self.name, self.address, self.email, self.student_number)

if __name__ == "__main__":
    import doctest
    doctest.testmod()
