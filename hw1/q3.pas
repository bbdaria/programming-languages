program ThirdQuestion;
type
    CourseType = (REG, LAB, SEM);


    Course = record
    typeOfCourse: CourseType;
    Id : Integer;
    name : string;
    case data : CourseType of
        REG : (numberOfLectures: Integer);
        LAB : (nameOfLecturer: string; place: string);
        SEM : (startTime: string; endTime: string)
    end;
Course_arr = array[0..99] of Course;
var
    courses : Course_arr;
    flag : boolean;
    coursetypee : string;
    typee : CourseType;
    id : Integer;
    name: string;
    data1: Integer;
    data2: string;
    data3: string;
    action: string;   

procedure add(var tp:CourseType; var id:Integer; var name:string;var data1:Integer; var data2:string; var data3:string);
begin
    courses[id-1000].typeOfCourse := tp;
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
            courses[id-1000].place := data3;
        end;
        SEM:
        begin
            courses[id-1000].startTime := data2;
            courses[id-1000].endTime := data3;
        end;
end;

begin
    flag := true;
    while flag do
    begin
        ReadLn(action);
        if action = 'ADD' then 
            begin
                ReadLn(coursetypee);
                ReadLn(id);
                ReadLn(name);
                    if coursetypee = 'REG' then
                        begin
                            ReadLn(data1);
                            typee := REG;
                            add(typee,id,name,data1,'buffer','buffer');
                        end;
                    if coursetypee = 'LAB' then
                        begin
                            ReadLn(data2);
                            ReadLn(data3);
                            typee := LAB;
                            add(typee,id,name,1,data2,data3);
                        end;
                    if coursetypee = 'SEM' then
                        begin
                            ReadLn(data2);
                            ReadLn(data3);
                            typee := SEM;
                            add(typee,id,name,1,data2,data3);
                        end;
            end;
        if action = 'PRINT' then
            // dariaa your code here
            ;
        if action = 'END' then flag:=false;

    end;
end.
