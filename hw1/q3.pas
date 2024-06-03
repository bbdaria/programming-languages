program ThirdQuestion;
type
    CourseType: (REG, LAB, SEM);


type Course = record
    typeOfCourse: CourseType;
    Id : Integer;
    name : string;
    case typeOfCourse : CourseType of
        REG : (numberOfLectures: Integer)
        LAB : (nameOfLecturer: string, place: string)
        SEM : (startTime, endTime: string)
    end;

begin




end.