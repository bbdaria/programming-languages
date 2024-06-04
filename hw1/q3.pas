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
    Course_arr = array[0..99] of Course;
    Course_set = set of 0..99;
var
    courses : Course_arr;
    courses_exist : Course_set; 
    flag : boolean;
    coursetype : CourseType;
    id : Integer;
    name: string;
    data1: Integer;
    data2: string;
    data3: string;
    action: string;   

procedure add(CourseType tp, Integer id, string name,Integer data1, string data2, sting data3)
begin
    courses[id-1000].typeOfCourse := REG;
    courses[id-1000].Id := id;
    courses[id-1000].name := name;
    case tp of
        REG: 
        begin
            courses[id-1000].numberOfLectures := data1;
        end;
        LAB:
        begin
            courses[id-1000].nameOfLecturer := data2;
            courses[id-1000].nameOfLecturer := data3;
        end;
        SEM:
        begin
            courses[id-1000].startTime := data2;
            courses[id-1000].endTime := data3;
        end;
    //courses_exist := courses_exist + [id];
end;

begin
flag := true;
while flag do
begin
    ReadLn(action);
    case action of
    "ADD":
        begin
            ReadLn(coursetype);
            ReadLn(id);
            ReadLn(name);
                case coursetype of
                "REG":
                    begin
                        ReadLn(data1);
                        add(REG,id,data1,"buffer","buffer")
                    end;
                "LAB":
                    begin
                        ReadLn(data2);
                        ReadLn(data3);
                        add(LAB,id,"buffer",data2,data3);
                    end;
                "SEM":
                    begin
                        ReadLn(data2);
                        ReadLn(data3);
                        add(SEM,id,"buffer",data2,data3);
                    end;
        end;
    "PRINT":
        // Dariaa your code here
        ;
    "END": flag:=false;

end;



end.
