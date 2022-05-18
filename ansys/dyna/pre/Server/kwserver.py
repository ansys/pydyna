import os
import sys
import traceback as tb
from concurrent import futures
import grpc
import kwprocess_pb2_grpc
import kwprocess_pb2

try:
    from keywordreader import KeywordReader as kp
    from keywordreader import KWD_GetDataType as gdt
    from keywordreader import KWD_SetDataType as sdt
    from keywordreader import KWD_OutType as ot
    from keywordreader import KWD_OutVersion as ver
    from keywordreader import KWD_PartProp as partprop
except ImportError as error:
    print(tb.print_exc())
    print(
        '\033[1;31m'
        'keywordreader cannot be found!\n'
        'Please check the libs path'
        '\033[0m'
    )
    sys.exit(0)

'''
class IGAServer(kwprocess_pb2_grpc.kwC2SServicer):
    def kwSetFileName(self, request, context):
        print (request.name)
        return kwprocess_pb2.kwFileNameReply(ret = True)
        

class Server():
    def run(self):
        server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
        kwprocess_pb2_grpc.add_kwC2SServicer_to_server(IGAServer(), server)
        server.add_insecure_port('[::]:50051')
        server.start()
        print ("kwgrpc Server listening on: localhost:50051")
        server.wait_for_termination()
'''
CHUNK_SIZE = 1024 * 1024
  
class IGAServer(kwprocess_pb2_grpc.kwC2SServicer):

    def __init__(self):
        self.fns = []
        self.fn = ''
        self.kwdproc = kp()
        
    def run(self):
        server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
        kwprocess_pb2_grpc.add_kwC2SServicer_to_server(self, server)
        server.add_insecure_port('[::]:50051')
        server.start()
        print ("kwgrpc Server listening on: localhost:50051")
        server.wait_for_termination()
        
    def kwSetFileName(self, request, context):
        filename = request.name
        num = request.num
        self.fns.append(filename)
        self.fn=filename
        return kwprocess_pb2.kwFileNameReply(ret = True)
        
    def get_file_chunks(self, filename):
        with open(filename, 'rb') as f:
            while True:
                piece = f.read(CHUNK_SIZE)
                if len(piece) == 0:
                    return
                yield kwprocess_pb2.Chunk(buffer=piece)
                
    def save_chunks_to_file(self, chunks, filename):
        with open(filename, 'wb') as f:
            for chunk in chunks:
                f.write(chunk.buffer)
        
    def Upload(self, request, context):
        filename = ""
        if len(self.fns) < 1:
            filename = "received_file"
        else:
            filename = self.fns[len(self.fns) - 1]
        newpath =  os.getcwd()+os.sep+'input' 
        if os.path.exists(newpath):
            pass
        else:  
            os.mkdir(newpath)
        fn=os.getcwd()+os.sep+'input'+os.sep+os.path.basename(filename)    
        self.save_chunks_to_file(request, fn)
        return kwprocess_pb2.kwFileReply(length=os.path.getsize(fn))
    
    def Download(self, request, context):
        return self.get_file_chunks(request.url)

    def LoadFile(self,request,context):
        path = os.getcwd()
        fn=path+os.sep+'input'+os.sep+os.path.basename(self.fns[0])
        self.kwdproc.read(fn)
        return kwprocess_pb2.LoadFileReply(ret = True)

    def SaveFile(self,request,context):
        path = os.getcwd()
        fn = path + os.sep+'output'
        if os.path.exists(fn):
            pass
        else:
            os.mkdir(fn)
        subsystems = self.kwdproc.get_data(gdt.KWD_GET_SUBSYSINFO)
        for iter in subsystems:
            filename = os.path.basename(subsystems[iter])
            subsystems[iter] = fn + os.sep+filename
        self.kwdproc.set(sdt.KWD_SET_OUTVERSION,15)
        if len(subsystems) == 1:
           self.kwdproc.write( subsystems)
        else:
           self.kwdproc.write(subsystems)
        print('Saved Successfully!')
        return kwprocess_pb2.SaveFileReply(length = 1)
    
    def CreateTimestep(self,request,context):
        tssfac = request.tssfac
        isdo=request.isdo
        dt2ms=request.dt2ms
        firstcard = '0.0,'+str(tssfac)+','+  str(isdo) + ",0.0," + str(dt2ms)
        newk = "*CONTROL_TIMESTEP\n"+ firstcard
        self.kwdproc.newkeyword(newk)
        print('Timestep Created...')
        return kwprocess_pb2.TimestepReply(answer = 0)

    def CreateTermination(self,request,context):
        endtim = request.endtim
        firstcard = str(endtim)+",0,0,0,1e8,0"
        newk = '*CONTROL_TERMINATION\n'+ firstcard
        self.kwdproc.newkeyword(newk)
        print('Termination Created...')
        return kwprocess_pb2.TerminationReply(answer = 0)

    def CreateControlContact(self,request,context):
        rwpnal = request.rwpnal
        shlthk = request.shlthk
        ssthk = request.ssthk
        ignore = request.ignore
        igactc = request.igactc
        card1 = '0,'+str(rwpnal)+",,"+str(shlthk)
        card2 = ",,,,,"+str(ssthk)
        card4 = str(ignore)
        card6 = '0, , , , , , ,'+str(igactc)
        newk = '*CONTROL_CONTACT\n'+card1+'\n'+card2+'\n\n'+card4+'\n\n'+card6
        self.kwdproc.newkeyword(newk)
        print('Control Contact Created...')
        return kwprocess_pb2.ControlContactReply(answer = 0)

    def CreateControlDiscreteElement(self,request,context):
        ndamp = request.ndamp
        tdamp = request.tdamp
        frics = request.frics
        fricr = request.fricr
        normk = request.normk
        sheark = request.sheark
        card1 = str(ndamp)+","+str(tdamp)+","+str(frics)+","+str(fricr)+","+str(normk)+","+str(sheark)
        newk = '*CONTROL_DISCRETE_ELEMENT\n'+card1+"\n0,0,0,0,0,0,6,0"
        self.kwdproc.newkeyword(newk)
        print('Control Discrete Element Created...')
        return kwprocess_pb2.ControlDiscreteElementReply(answer = 0)

    def CreateControlAccuracy(self,request,context):
        osu = request.osu
        inn = request.inn
        pidosu = request.pidosu
        iacc = request.iacc
        exacc = request.exacc
        card1 = str(osu)+","+str(inn)+","+str(pidosu)+","+str(iacc)+","+str(exacc)
        newk = '*CONTROL_ACCURACY\n'+card1
        self.kwdproc.newkeyword(newk)
        print('Control Accuracy Created...')
        return kwprocess_pb2.ControlAccuracyReply(answer = 0)

    def CreateControlEnergy(self,request,context):
        hgen = request.hgen
        rwen = request.rwen
        slnten = request.slnten
        rylen = request.rylen
        irgen = request.irgen
        card1 = str(hgen)+","+str(rwen)+","+str(slnten)+","+str(rylen)+","+str(irgen)
        newk = '*CONTROL_ENERGY\n'+card1
        self.kwdproc.newkeyword(newk)
        print('Control Energy Created...')
        return kwprocess_pb2.ControlEnergyReply(answer = 0)

    def CreateControlShell(self,request,context):
        wrpang = request.wrpang
        esort = request.esort
        irnxx = request.irnxx
        istupd = request.istupd
        theory = request.theory
        bwc = request.bwc
        miter = request.miter
        proj = request.proj
        irquad = request.irquad
        card1 = str(wrpang)+","+str(esort)+","+str(irnxx)+","+str(istupd)+","+str(theory)+","+str(bwc)+","+str(miter)+","+str(proj)
        card3 = ",,,,"+str(irquad)
        newk = '*CONTROL_SHELL\n'+card1+"\n\n"+card3
        self.kwdproc.newkeyword(newk)
        print('Control Shell Created...')
        return kwprocess_pb2.ControlShellReply(answer = 0)

    def CreateControlSolid(self,request,context):
        esort = request.esort
        fmatrx = request.fmatrx
        niptets = request.niptets
        swlocl = request.swlocl
        psfail = request.psfail
        t10jtol = request.t10jtol
        icoh = request.icoh
        tet13k = request.tet13k
        card1 = str(esort)+","+str(fmatrx)+","+str(niptets)+","+str(swlocl)+","+str(psfail)+","+str(t10jtol)+","+str(icoh)+","+str(tet13k)
        newk = '*CONTROL_SOLID\n'+card1
        self.kwdproc.newkeyword(newk)
        print('Control Solid Created...')
        return kwprocess_pb2.ControlSolidReply(answer = 0)

    def CreateDBBinary(self,request,context):
        dt = request.dt
        card1 = str(dt)+', , , , ,'
        newk = '*DATABASE_BINARY_D3PLOT\n' + card1
        self.kwdproc.newkeyword(newk)
        print('DB Binary Created...')
        maxint = request.maxint
        dcomp = request.dcomp
        nintsld = request.nintsld
        if maxint!=3 or dcomp!=1 or nintsld!=1:
            card1= ",,"+str(maxint)
            card2 = ",,,"+str(dcomp)
            card3 = str(nintsld)
            newk = "*DATABASE_EXTENT_BINARY\n"+card1+"\n"+card2+"\n"+card3
            self.kwdproc.newkeyword(newk)
        return kwprocess_pb2.DBBinaryReply(answer = 0)

    def CreateDBAscii(self,request,context):
        type = request.type
        dt = request.dt
        binary = request.binary
        lcur = request.lcur
        ioopt = request.ioopt
        opcode = "*DATABASE_"+type.upper()
        card1 = str(dt)+","+str(binary)+","+str(lcur)+","+str(ioopt)
        newk = opcode +"\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = "Database "+ type +"Created..."
        print(msg)
        return kwprocess_pb2.DBAsciiReply(answer = 0)

        #INITIAL
    def CreateInitVelRigidBody(self,request,context):
        pid = request.pid
        vx = request.vx
        vy = request.vy
        vz = request.vz
        vxr = request.vxr
        vyr = request.vyr
        vzr = request.vzr
        lcid = request.lcid
        opcode ="*INITIAL_VELOCITY_RIGID_BODY"
        card1 = str(pid)+","+str(vx)+","+str(vy)+","+str(vz)+","+str(vxr)+","+str(vyr)+","+str(vzr)+","+str(lcid)
        newk = opcode +"\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = "*INITIAL_VELOCITY_RIGID_BODY Created..."
        print(msg)
        return kwprocess_pb2.InitVelRigidBodyReply(answer = 0)

    def CreateInitVelGeneration(self,request,context):
        id = request.id
        styp = request.styp
        omega = request.omega
        vx = request.vx
        vy = request.vy
        vz = request.vz
        xc = request.xc
        yc = request.yc
        zc = request.zc
        nx = request.nx
        ny = request.ny
        nz = request.nz
        phase = request.phase
        opcode ="*INITIAL_VELOCITY_GENERATION"
        card1 = str(id)+","+str(styp)+","+str(omega)+","+str(vx)+str(vy)+","+str(vz)
        card2 = str(xc)+","+str(yc)+","+str(zc)+","+str(nx)+","+str(ny)+","+str(nz)+","+str(phase)
        newk = opcode +"\n" + card1 +"\n"+card2
        self.kwdproc.newkeyword(newk)
        msg = "*INITIAL_VELOCITY_GENERATION Created..."
        print(msg)
        return kwprocess_pb2.InitVelGenerationReply(answer = 0)

    def CreateInitVelGenerationStartTime(self,request,context):
        stime = request.stime
        opcode ="*INITIAL_VELOCITY_GENERATION_START_TIME"
        card1 = str(stime)
        newk = opcode +"\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = "*INITIAL_VELOCITY_GENERATION_START_TIME Created..."
        print(msg)
        return kwprocess_pb2.InitVelGenerationStartTimeReply(answer = 0)

    def CreateDefineCurve(self,request,context):
        lcid = request.lcid
        sfo = request.sfo
        abscissa = request.abscissa
        ordinate = request.ordinate
        card1 = str(lcid)+', , 1,'+str(sfo)
        newk = '*DEFINE_CURVE\n'+card1+"\n"
        for index in range(len(abscissa)):
            repeatcard = str(abscissa[index]) + "," + str(ordinate[index])+"\n"
            newk += repeatcard
        self.kwdproc.newkeyword(newk)
        msg = 'DefineCurve '+str(lcid)+'Created...'
        print(msg)
        return kwprocess_pb2.DefineCurveReply(answer = 0)   

    def CreateDefineVector(self,request,context):
        title = request.title
        vid = request.vid
        xt = request.xt
        yt = request.yt
        zt = request.zt
        xh = request.xh
        yh = request.yh
        zh = request.zh
        card1 = str(vid)+","+str(xt)+","+str(yt)+","+str(zt)+","+str(xh)+","+str(yh)+","+str(zh)+",0"
        opcode = "*DEFINE_VECTOR"
        newk = opcode
        if(len(title)>0):
            newk += "_TITLE" +"\n"
            newk += title
        newk += "\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'DefineVector '+str(vid)+'Created...'
        print(msg)
        return kwprocess_pb2.DefineVectorReply(answer = 0) 

    def CreateDefineDEMeshSurface(self,request,context):
        sid = request.sid
        type = request.type
        nquad = request.nquad
        despid = request.despid
        desxid = request.desxid
        nsid = request.nsid
        rsf = request.rsf
        card1 = str(sid)+","+str(type)+","+str(nquad)+","+str(despid)+","+str(desxid)+","+str(nsid)+","+str(rsf)
        opcode = "*DEFINE_DE_MESH_SURFACE"
        newk = opcode +"\n"+card1
        self.kwdproc.newkeyword(newk)
        msg = 'DEFINE_DE_MESH_SURFACE Created...'
        print(msg)
        return kwprocess_pb2.DefineDEMeshSurfaceReply(answer = 0) 

    def CreatePartSet(self,request,context):
        sid = request.sid
        pids = request.pids
        if sid==0:
            lastid = self.kwdproc.get_data(gdt.KWD_PARTSET_LASTID)
            sid = lastid+1
        card1 = str(sid)
        newk =  "*SET_PART_LIST\n" + card1 + "\n";  
        repeatcard = ''
        count = 0
        for pid in pids:
            repeatcard += str(pid)
            count+=1
            if count%8==0 or count >= len(pids):
                repeatcard += '\n'
                newk += repeatcard
                repeatcard=''
                continue
            repeatcard+=','
        self.kwdproc.newkeyword(newk)
        msg = 'PartSet '+str(sid)+'Created...'
        print(msg)
        return kwprocess_pb2.PartSetReply(answer = sid)

    def CreateShellSet(self,request,context):
        option = request.option
        title = request.title
        sid = request.sid
        eids = request.eids
        opcode = "*SET_SHELL"
        if len(opcode)>0:
            opcode += "_"+option.upper()
        if len(title)>0:
            opcode += "_TITLE"
        newk = opcode
        if len(title)>0:
            newk += "\n" + title
        card1 = str(sid)
        newk +=  "\n" + card1 + "\n";  
        repeatcard = ''
        count = 0
        for eid in eids:
            repeatcard += str(eid)
            count+=1
            if count%8==0 or count >= len(eids):
                repeatcard += '\n'
                newk += repeatcard
                repeatcard=''
                continue
            repeatcard+=','
        self.kwdproc.newkeyword(newk)
        msg = 'ShellSet '+str(sid)+' Created...'
        print(msg)
        return kwprocess_pb2.ShellSetReply(answer = 0)

    def CreateSolidSet(self,request,context):
        title = request.title
        sid = request.sid
        ki = request.ki
        opcode = "*SET_SOLID"
        if len(title)>0:
            opcode += "_TITLE"
        newk = opcode
        if len(title)>0:
            newk += "\n"+title
        card1 = str(sid)
        newk =  "\n" + card1 + "\n";  
        repeatcard = ''
        count = 0
        for k in ki:
            repeatcard += str(k)
            count+=1
            if count%8==0 or count >= len(ki):
                repeatcard += '\n'
                newk += repeatcard
                repeatcard=''
                continue
            repeatcard+=','
        self.kwdproc.newkeyword(newk)
        msg = 'SET_SOLID '+str(sid)+' Created...'
        print(msg)
        return kwprocess_pb2.SolidSetReply(answer = 0)  

    def CreateNodeSet(self,request,context):
        option = request.option
        sid = request.sid
        genoption = request.genoption
        entities = request.entities
        opcode = "*SET_NODE"
        if len(option)>0:
            opcode += "_"+option.upper()
        newk = opcode
        card1 = str(sid)
        card2 = genoption
        for i in range(len(entities)):
            card2 += ","+str(entities[i])
        newk +=  "\n" + card1 + "\n"+card2 
        self.kwdproc.newkeyword(newk)
        msg = 'SET_NODE '+str(sid)+' Created...'
        print(msg)
        return kwprocess_pb2.NodeSetReply(answer = 0) 

    def CreateSegmentSet(self,request,context):
        #title = request.title
        sid = request.sid
        solver = request.solver
        n1 = request.n1
        n2 = request.n2
        n3 = request.n3
        n4 = request.n4
        opcode = "*SET_SEGMENT"
        #if len(title)>0:
        #    opcode += "_TITLE"
        newk = opcode
        #if len(title)>0:
        #    newk += "\n"+title
        card1 = str(sid)+",,,,,"+solver
        newk = opcode + "\n" + card1 
        repeatcard = ''
        count = 0
        for i in range(len(n1)):
            repeatcard+= "\n"+str(n1[i])+","+str(n2[i])+","+str(n3[i])+","+str(n4[i])
        newk += repeatcard
        self.kwdproc.newkeyword(newk)
        msg = 'SET_SEGMENT '+str(sid)+' Created...'
        print(msg)
        return kwprocess_pb2.SegmentSetReply(answer = 0) 
    
    def CreateRigidWallGeom(self,request,context):
        geomtype = request.geomtype
        motion = request.motion
        display = request.display
        parameter = request.parameter
        lcid = request.lcid
        vx = request.vx
        vy = request.vy
        vz = request.vz
        if geomtype == 3:
            card2 = str(parameter[0])+','+str(parameter[1])+','+str(parameter[2])+','+str(parameter[3])+','+str(parameter[4])+','+str(parameter[5])
            card3 = str(parameter[6])+','+str(parameter[7])
            if motion != 0:
                card4 = str(lcid)+',0,'+str(vx)+','+str(vy)+','+str(vz)
            opcode = '*RIGIDWALL_GEOMETRIC_CYLINDER'
            if motion != 0:
                opcode+='_MOTION'
            if display != 0:
                opcode += '_DISPLAY'
            newk = opcode+"\n0,0,0\n" + card2 + "\n"+card3
            if motion !=0:
                newk += ('\n'+card4)
            if display !=0:
                newk += "\n ,1e-9,1e-4,0.3"
            self.kwdproc.newkeyword(newk)
        print('Cylinder Rigidwall Geometric Created...')   
        return kwprocess_pb2.RigidWallGeomReply(answer = 0)  

    def CreateContact(self,request,context):
        cid = request.cid
        title = request.title
        option1 = request.option1
        #option2 = request.option2
        option3 = request.option3
        ssid = request.ssid
        msid = request.msid
        sstyp = request.sstyp
        mstyp = request.mstyp
        #card2
        fs = request.fs
        fd = request.fd
        vdc = request.vdc
        penchk = request.penchk
        birthtime = request.birthtime
        #card3
        sst = request.sst
        mst = request.mst
        #card4
        optionres = request.optionres
        nfls = request.nfls
        sfls = request.sfls
        param = request.param
        ct2cn = request.ct2cn
        #optiona
        soft = request.soft
        sofscl = request.sofscl
        lcidab = request.lcidab
        maxpar = request.maxpar
        sbopt = request.sbopt
        depth = request.depth
        bsort = request.bsort
        frcfrq = request.frcfrq
        #optionc
        igap = request.igap
        opcode = "*CONTACT_"+option1.upper()
        if option3:
            opcode += "_ID"
            card0 = str(cid)+","+title
            opcode += "\n" + card0
        card1 = str(ssid)+","+ str(msid) + "," + str(sstyp) + "," + str(mstyp);  
        card2 = str(fs)+"," +str(fd)+",,,"+str(vdc)+","+str(penchk)+","+ str(birthtime)
        card3 = "1,1,"+str(sst)+"," +str(mst)+",1,1,1,1"
        card4 = str(optionres)+","+str(nfls)+","+str(sfls)+","+str(param)+",,,"+str(ct2cn)
        carda = str(soft)+","+ str(sofscl) + "," + str(lcidab) + "," + str(maxpar)+","+str(sbopt)+","+ str(depth) + "," + str(bsort) + "," + str(frcfrq); 
        cardb = "0,1,2,0,0,0,0.5,0"
        cardc = str(igap)
        if option1 == "TIED_SHELL_EDGE_TO_SURFACE":
            newk = opcode+"\n"+card1+"\n"+card2+"\n"+card3
        if option1 == "AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK" :
            newk = opcode+"\n"+card1+"\n"+card2+"\n"+card3+"\n"+card4+"\n"+carda
        if option1 == "AUTOMATIC_SINGLE_SURFACE_SMOOTH":
            newk = opcode+"\n"+card1+"\n"+card2+"\n"+card3+"\n"+carda+"\n"+cardb+"\n"+cardc  
        self.kwdproc.newkeyword(newk)
        print('Contact  Created...')
        return kwprocess_pb2.ContactReply(answer = 0)

    def CreateContactAutomatic(self,request,context):
        ssid = request.ssid
        msid = request.msid
        sstyp = request.sstyp
        mstyp = request.mstyp
        option = request.option
        card1 =  str(ssid)+","+ str(msid) + "," + str(sstyp) + "," + str(mstyp);  
        card4 = str(option)
        newk = "*CONTACT_AUTOMATIC_SINGLE_SURFACE\n" + card1+"\n\n\n"+card4
        self.kwdproc.newkeyword(newk)
        print('Contact Automatic  Created...')
        return kwprocess_pb2.ContactAutomaticReply(answer = 0)

    def CreateContactTied(self,request,context):
        ssid = request.ssid
        msid = request.msid
        sstyp = request.sstyp
        mstyp = request.mstyp
        card1 = str(ssid) + "," + str(msid) + "," + str(sstyp) + "," + str(mstyp)
        newk = "*CONTACT_TIED_SHELL_EDGE_TO_SURFACE_OFFSET\n" + card1+"\n0,0,0,0\n1,1"
        self.kwdproc.newkeyword(newk)
        print('Contact Tied  Created...')
        return kwprocess_pb2.ContactTiedReply(answer = 0)

    #BOUNDARY
    def CreateBdyPrescribedMotion(self,request,context):
        id = request.id
        heading = request.heading
        option = request.option 
        typeid = request.typeid
        dof = request.dof
        vad = request.vad
        lcid = request.lcid
        sf = request.sf
        vid = request.vid
        birth=request.birth
        card0 = str(id)+","+heading
        card1 = str(typeid) + "," + str(dof) + "," + str(vad) + "," + str(lcid)+ "," + str(sf) + "," + str(vid)+",,"+str(birth)
        opcode = "*BOUNDARY_PRESCRIBED_MOTION_"+option.upper()
        newk = opcode+"\n" + card1
        self.kwdproc.newkeyword(newk)
        print('*BOUNDARY_PRESCRIBED_MOTION  Created...')
        return kwprocess_pb2.BdyPrescribedMotionReply(answer = 0) 

    def CreateBdySpc(self,request,context):
        #id = request.id
        #heading = request.heading
        option1 = request.option1
        birthdeath = request.birthdeath
        nid = request.nid 
        cid = request.cid
        dofx = request.dofx
        dofy = request.dofy
        dofz = request.dofz
        dofrx = request.dofrx
        dofry = request.dofry
        dofrz = request.dofrz
        birth = request.birth
        death = request.death
        #card0 = str(id)+","+heading
        card1 = str(nid) + "," + str(cid) + "," + str(dofx) + "," + str(dofy)+ "," + str(dofz) + "," + str(dofrx)+ "," + str(dofry) + "," + str(dofrz)
        card2 = str(birth)+","+str(death)
        opcode = "*BOUNDARY_SPC_"+option1.upper()
        if birthdeath:
            opcode+="_BIRTH_DEATH"
        #if id>0:
        #    opcode+="_ID"
        newk = opcode +"\n"
        #if id>0:
        #    newk += card0+"\n"
        newk += card1 +"\n"
        if birthdeath:
            newk += card2 
        self.kwdproc.newkeyword(newk)
        msg = "*BOUNDARY_SPC Created..."
        print(msg)
        return kwprocess_pb2.BdySpcReply(answer = 0)  

    #CONSTRAINED
    def CreateConstrainedExtraNodes(self,request,context):
        option = request.option
        pid = request.pid
        nid = request.nid
        iflag = request.iflag
        card1 = str(pid) + "," + str(nid) + "," + str(iflag)
        opcode = "*CONSTRAINED_EXTRA_NODES_"+option.upper()
        newk = opcode+"\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*CONSTRAINED_EXTRA_NODES Created...'
        print(msg)
        return kwprocess_pb2.ConstrainedExtraNodesReply(answer = 0)   

    #LOAD
    def CreateLoadBody(self,request,context):
        option = request.option
        lcid = request.lcid
        card1 = str(lcid)
        opcode = "*LOAD_BODY_"+option.upper()
        newk = opcode+"\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.LoadBodyReply(answer = 0)    

    #MATERIAL
    def CreateMatRigid(self,request,context):
        mid = request.mid
        ro = request.ro
        e = request.e
        pr = request.pr
        cmo = request.cmo
        con1 = request.con1
        con2 = request.con2
        card1 = str(mid)+","+str(ro)+","+str(e)+","+str(pr)
        card2 = str(cmo)
        if cmo == 1 or cmo == -1:
            card2 += ","+str(con1)+","+str(con2)
        opcode = "*MAT_RIGID"
        newk = opcode +"\n"+card1+"\n"+card2+"\n"
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatRigidReply(ret = 0)  

    def CreateMatElastic(self,request,context):
        mid = request.mid
        ro = request.ro
        e = request.e
        pr = request.pr
        card1 = str(mid)+","+str(ro)+","+str(e)+","+str(pr)
        opcode = "*MAT_ELASTIC"
        newk = opcode +"\n"+card1
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatElasticReply(ret = 0)      

    def CreateSectionIGAShell(self,request,context):
        secid = request.secid
        elform = request.elform
        shrf = request.shrf
        thickness = request.thickness
        card1 = str(secid) + "," + str(elform) + "," + str(shrf)
        card2 = str(thickness)
        newk = "*SECTION_IGA_SHELL\n" + card1+"\n"+card2
        self.kwdproc.newkeyword(newk)
        msg = 'Section IGAShell '+str(secid)+' Created...'
        print(msg)
        return kwprocess_pb2.SectionIGAShellReply(answer = 0)

    def CreateSectionShell(self,request,context):
        secid = request.secid
        elform = request.elform
        shrf = request.shrf
        nip = request.nip
        propt = request.propt
        t1 = request.t1
        t2 = request.t2
        t3 = request.t3
        t4 = request.t4
        card1 = str(secid) + "," + str(elform)+ "," + str(shrf)+ "," + str(nip)+ "," + str(propt)
        card2 = str(t1)+ "," + str(t2)+ "," + str(t3)+ "," + str(t4)
        newk = "*SECTION_SHELL\n" + card1 +"\n"+card2
        self.kwdproc.newkeyword(newk)
        msg = 'Section Shell '+str(secid)+' Created...'
        print(msg)
        return kwprocess_pb2.SectionShellReply(answer = 0)
    
    def CreateSectionSolid(self,request,context):
        secid = request.secid
        elform = request.elform
        card1 = str(secid) + "," + str(elform)
        newk = "*SECTION_SOLID\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'Section Solid '+str(secid)+' Created...'
        print(msg)
        return kwprocess_pb2.SectionSolidReply(answer = 0)

    def CreateHourglass(self,request,context):
        ghid = request.ghid
        ihq = request.ihq
        qm = request.qm
        q1 = request.q1
        q2 = request.q2
        qb = request.qb
        qw = request.qw
        card1 = str(ghid) + "," + str(ihq) + "," + str(qm)+", ,"+str(q1) + "," + str(q2) + "," + str(qb) + "," + str(qw)
        newk = "*HOURGLASS\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'Hourglass '+str(ghid)+' Created...'
        print(msg)
        return kwprocess_pb2.SectionSolidReply(answer = 0)

    def SetPartProperty(self,request,context):
        pid = request.pid
        secid = request.secid
        mid = request.mid
        eosid = request.eosid
        hgid = request.hgid
        grav = request.grav
        adpopt = request.adpopt
        tmid = request.tmid
        pp = [secid,mid,eosid,hgid,grav,adpopt,tmid]
        self.kwdproc.setpartproperty(gdt.KWD_DETAILDATA_EXTERNALID, pid, pp)
        msg = 'Set Property for Part '+str(pid)
        print(msg)
        return kwprocess_pb2.PartPropertyReply(answer = 0)

    def GetSolidElements(self,request,context):
        coords = self.kwdproc.get_data_nodearray()
        nids = []
        for coord in coords:
            nids.append(coord.id())
        cons = []
        elements = self.kwdproc.get_data_solidarray()
        for element in elements:
            for i in range(8):
                cons.append(element.Node(i))
        return kwprocess_pb2.GetSolidElementsReply(nodeids = cons)

    def GetNodes(self,request,context):
        coords = self.kwdproc.get_data_nodearray() 
        nodes = []
        for coord in coords: 
            nodes.append(coord.X()) 
            nodes.append(coord.Y())
            nodes.append(coord.Z())
        return kwprocess_pb2.GetNodesReply(coords = nodes) 

    def ICFDCreateControlTime(self,request,context):     
        tim = request.tim
        dt = request.dt
        card1 = str(tim) + "," + str(dt)+",1"
        newk = "*ICFD_CONTROL_TIME\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD Control time Created...'
        print(msg)
        return kwprocess_pb2.ICFDControlTimeReply(answer = 0) 

    def ICFDCreateControlGeneral(self,request,context):     
        atype = request.atype
        mtype = request.mtype
        dvcl = request.dvcl
        rdvcl = request.rdvcl
        card1 = str(atype) + "," + str(mtype)+ "," + str(dvcl)+ "," + str(rdvcl)
        newk = "*ICFD_CONTROL_GENERAL\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD Control General Created...'
        print(msg)
        return kwprocess_pb2.ICFDControlGeneralReply(answer = 0) 

    def ICFDCreateControlOutput(self,request,context):     
        msgl = request.msgl
        card1 = str(msgl)
        newk = "*ICFD_CONTROL_OUTPUT\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD Control Output Created...'
        print(msg)
        return kwprocess_pb2.ICFDControlOutputReply(answer = 0) 

    def ICFDCreateControlTurbulence(self,request,context): 
        tmod = request.tmod    
        card1 = str(tmod)
        newk = "*ICFD_CONTROL_TURBULENCE\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD Control Turbulence Created...'
        print(msg)
        return kwprocess_pb2.ICFDControlTurbulenceReply(answer = 0) 

    def ICFDCreateControlDEMCoupling(self,request,context): 
        ctype = request.ctype
        bt = request.bt
        dt = request.dt
        sf = request.sf    
        card1 = str(ctype)+","+str(bt)+","+str(dt)+","+str(sf)
        newk = "*ICFD_CONTROL_DEM_COUPLING\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD Control DEM Coupling Created...'
        print(msg)
        return kwprocess_pb2.ICFDControlDEMCouplingReply(answer = 0) 

    def ICFDCreateSection(self,request,context):     
        sid = request.sid
        card1 = str(sid)
        newk = "*ICFD_SECTION\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD section '+str(sid)+' Created...'
        print(msg)
        return kwprocess_pb2.ICFDSectionReply(answer = 0)

    def ICFDCreateMat(self,request,context):     
        mid = request.mid
        flg = request.flg
        ro = request.ro
        vis = request.vis
        card1 = str(mid) + "," + str(flg)+ "," + str(ro)+ "," + str(vis)
        newk = "*ICFD_MAT\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD material '+str(mid)+' Created...'
        print(msg)
        return kwprocess_pb2.ICFDMatReply(answer = 0)    

    def ICFDCreatePart(self,request,context):     
        pid = request.pid
        secid = request.secid
        mid = request.mid
        card1 = str(pid) + "," + str(secid)+ "," + str(mid)
        newk = "*ICFD_PART\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD part '+str(pid)+' Created...'
        print(msg)
        return kwprocess_pb2.ICFDPartReply(answer = 0)      
    
    def ICFDCreatePartVol(self,request,context):     
        pid = request.pid
        secid = request.secid
        mid = request.mid
        spids = request.spids
        card1 = str(pid) + "," + str(secid)+ "," + str(mid)
        newk =  "*ICFD_PART_VOL\n" + card1 + "\n";  
        repeatcard = ''
        count = 0
        for spid in spids:
            repeatcard += str(spid)
            count+=1
            if count%8==0 or count >= len(spids):
                repeatcard += '\n'
                newk += repeatcard
                repeatcard=''
                continue
            repeatcard+=','
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD part vol '+str(pid)+' Created...'
        print(msg)
        return kwprocess_pb2.ICFDPartVolReply(answer = 0) 

    def ICFDCreateDBDrag(self,request,context):     
        pid = request.pid
        card1 = str(pid)
        newk = "*ICFD_DATABASE_DRAG\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD database drag '+str(pid)+' Created...'
        print(msg)
        return kwprocess_pb2.ICFDDBDragReply(answer = 0) 

    def ICFDCreateBdyPrescribedVel(self,request,context):     
        pid = request.pid
        dof = request.dof
        vad = request.vad
        lcid = request.lcid
        card1 = str(pid) + "," + str(dof)+ "," + str(vad)+ "," + str(lcid)+",1"
        newk = "*ICFD_BOUNDARY_PRESCRIBED_VEL\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD boundary prescribed vel '+str(pid)+' Created...'
        print(msg)
        return kwprocess_pb2.ICFDBdyPrescribedVelReply(answer = 0)

    def ICFDCreateBdyPrescribedPre(self,request,context):     
        pid = request.pid
        lcid = request.lcid
        card1 = str(pid) + "," + str(lcid)
        newk = "*ICFD_BOUNDARY_PRESCRIBED_PRE\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD boundary prescribed pre '+str(pid)+' Created...'
        print(msg)
        return kwprocess_pb2.ICFDBdyPrescribedPreReply(answer = 0)

    def ICFDCreateSolverTolMMOV(self,request,context):     
        atol = request.atol
        rtol = request.rtol
        card1 = str(atol) + "," + str(rtol)
        newk = "*ICFD_SOLVER_TOL_MMOV\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD Solver Tol MMOV Created...'
        print(msg)
        return kwprocess_pb2.ICFDSolverTolMMOVReply(answer = 0)

    def ICFDCreateBdyFreeSlip(self,request,context):     
        pid = request.pid
        card1 = str(pid)
        newk = "*ICFD_BOUNDARY_FREESLIP\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD boundary freeslip '+str(pid)+' Created...'
        print(msg)
        return kwprocess_pb2.ICFDBdyFreeSlipReply(answer = 0)

    def ICFDCreateBdyNonSlip(self,request,context):     
        pid = request.pid
        card1 = str(pid)
        newk = "*ICFD_BOUNDARY_NONSLIP\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD boundary nonslip '+str(pid)+' Created...'
        print(msg)
        return kwprocess_pb2.ICFDBdyNonSlipReply(answer = 0)

    def MESHCreateVolume(self,request,context):     
        volid = request.volid
        pids = request.pids
        card1 = str(volid)
        newk =  "*MESH_VOLUME\n" + card1 + "\n";  
        repeatcard = ''
        count = 0
        for pid in pids:
            repeatcard += str(pid)
            count+=1
            if count%8==0 or count >= len(pids):
                repeatcard += '\n'
                newk += repeatcard
                repeatcard=''
                continue
            repeatcard+=','
        self.kwdproc.newkeyword(newk)
        msg = 'MESH volume '+str(volid)+' Created...'
        print(msg)
        return kwprocess_pb2.MeshVolumeReply(answer = 0)

    def MESHCreateEmbedShell(self,request,context):     
        volid = request.volid
        pids = request.pids
        card1 = str(volid)
        newk =  "*MESH_EMBEDSHELL\n" + card1 + "\n";  
        repeatcard = ''
        count = 0
        for pid in pids:
            repeatcard += str(pid)
            count+=1
            if count%8==0 or count >= len(pids):
                repeatcard += '\n'
                newk += repeatcard
                repeatcard=''
                continue
            repeatcard+=','
        self.kwdproc.newkeyword(newk)
        msg = 'MESH Embed Shell '+str(volid)+' Created...'
        print(msg)
        return kwprocess_pb2.MeshEmbedShellReply(answer = 0)

    def MESHCreateSizeShape(self,request,context):     
        sname = request.sname
        force = request.force
        method = request.method
        msize = request.msize
        parameter = request.parameter
        card1 = sname.upper() + ","+str(force) + "," + str(method)
        newk = "*MESH_SIZE_SHAPE\n" + card1
        #if sname.upper() == "BOX":
        newk += "\n"+str(msize)
        for i in range(len(parameter)):
            newk += "," + str(parameter[i])
        self.kwdproc.newkeyword(newk)
        msg = 'MESH_SIZE_SHAPE Created...'
        print(msg)
        return kwprocess_pb2.MeshSizeShapeReply(answer = 0) 

    def MESHCreateBl(self,request,context):     
        pid = request.pid
        nelth = request.nelth
        card1 = str(pid) + "," + str(nelth)
        newk = "*MESH_BL\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'MESH bl '+str(pid)+' Created...'
        print(msg)
        return kwprocess_pb2.MeshBlReply(answer = 0)   

    def CreateDampingGlobal(self,request,context):     
        lcid = request.lcid
        valdmp = request.valdmp
        card1 = str(lcid) + "," + str(valdmp)
        newk = "*DAMPING_GLOBAL\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*DAMPING_GLOBAL Created...'
        print(msg)
        return kwprocess_pb2.DampingGlobalReply(answer = 0) 

    def CreateDampingPartStiffness(self,request,context): 
        isset = request.isset    
        id = request.id
        coef = request.coef
        opcode = "*DAMPING_PART_STIFFNESS"
        if isset:
            opcode+="_SET"
        card1 = str(id) + "," + str(coef)
        newk = opcode+"\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = opcode + ' Created...'
        print(msg)
        return kwprocess_pb2.DampingPartStiffnessReply(answer = 0) 

if __name__ == '__main__':
    server = IGAServer()
    server.run()