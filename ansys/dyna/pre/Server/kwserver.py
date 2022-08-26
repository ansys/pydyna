import os
import sys
import traceback as tb
import platform
from concurrent import futures
import grpc
import kwprocess_pb2_grpc
import kwprocess_pb2

cwd = os.getcwd()
if(platform.system() == 'Windows'):
    if(sys.version_info[0]==3 and sys.version_info[1]==6):
        sys.path.insert(0, os.path.normpath(os.path.join(
            cwd,'lib','windows','cp36'
        )))
    elif(sys.version_info[0]==3 and sys.version_info[1]==8):
        sys.path.insert(0, os.path.normpath(os.path.join(
            cwd,'lib','windows','cp38'
        )))
    elif(sys.version_info[0]==3 and sys.version_info[1]==9):
        sys.path.insert(0, os.path.normpath(os.path.join(
            cwd,'lib','windows','cp39'
        )))
else:
    sys.path.insert(0, os.path.normpath(os.path.join(
        cwd, 'lib', 'linux'
    )))

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
        self.fns.clear()
        print('Saved Successfully!')
        return kwprocess_pb2.SaveFileReply(length = 1)
    
    def CreateTimestep(self,request,context):
        tssfac = request.tssfac
        isdo=request.isdo
        dt2ms=request.dt2ms
        firstcard = '0.0,'+str(tssfac)+','+  str(isdo) + ",0.0," + str(dt2ms)+",0,0,0"
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

    def CreateControlOutput(self,request,context):
        npopt = request.npopt
        neecho = request.neecho
        card1 = str(npopt)+","+str(neecho)
        newk = '*CONTROL_OUTPUT\n'+card1
        self.kwdproc.newkeyword(newk)
        print('Control Output Created...')
        return kwprocess_pb2.ControlOutputReply(answer = 0)

    def CreateControlContact(self,request,context):
        rwpnal = request.rwpnal
        shlthk = request.shlthk
        orien = request.orien
        ssthk = request.ssthk
        ignore = request.ignore
        igactc = request.igactc
        card1 = '0,'+str(rwpnal)+",,"+str(shlthk)+",,,"+str(orien)
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

    def CreateControlBulkViscosity(self,request,context):
        q1=request.q1
        q2=request.q2
        type = request.type
        card1=str(q1)+","+str(q2)+","+str(type)+",0,0"
        newk='*CONTROL_BULK_VISCOSITY\n'+card1
        self.kwdproc.newkeyword(newk)
        print('Control Bulk Viscosity Created...')
        return kwprocess_pb2.ControlBulkViscosityReply(answer = 0)

    def CreateControlHourgalss(self,request,context):
        ihq=request.ihq
        qh=request.qh
        card1=str(ihq)+","+str(qh)
        newk='*CONTROL_HOURGLASS\n'+card1
        self.kwdproc.newkeyword(newk)
        print('Control Hourglass Created...')
        return kwprocess_pb2.ControlHourglassReply(answer = 0)

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

    def CreateControlImplicitGeneral(self,request,context):
        imflag = request.imflag
        dt0 = request.dt0
        card1 = str(imflag)+","+str(dt0)+",2,1,2,0,0,0"
        newk = '*CONTROL_IMPLICIT_GENERAL\n'+card1
        self.kwdproc.newkeyword(newk)
        print('Control Implicit General Created...')
        return kwprocess_pb2.ControlImplicitGeneralReply(answer = 0)

    def CreateControlImplicitAuto(self,request,context):
        iauto = request.iauto
        iteopt = request.iteopt
        card1 = str(iauto)+","+str(iteopt)+",5,0,0,0"
        newk = '*CONTROL_IMPLICIT_AUTO\n'+card1
        self.kwdproc.newkeyword(newk)
        print('Control Implicit Auto Created...')
        return kwprocess_pb2.ControlImplicitAutoReply(answer = 0)
    
    def CreateControlImplicitDynamic(self,request,context):
        imass = request.imass
        gamma = request.gamma
        beta = request.beta
        card1 = str(imass)+","+str(gamma)+","+str(beta)+",0,1e28,1e28,0,0"
        newk = '*CONTROL_IMPLICIT_DYNAMIC\n'+card1
        self.kwdproc.newkeyword(newk)
        print('Control Implicit Dynamic Created...')
        return kwprocess_pb2.ControlImplicitDynamicReply(answer = 0)

    def CreateControlImplicitEigenvalue(self,request,context):
        neig = request.neig
        shfscl = request.shfscl
        card1 = str(neig)+",0,0,-1e29,0,1e29,2,0"
        card2 = "0,0,0,0,0,0,"+str(shfscl)
        newk = '*CONTROL_IMPLICIT_EIGENVALUE\n'+card1 +'\n' +card2
        self.kwdproc.newkeyword(newk)
        print('Control Implicit Eigenvalue Created...')
        return kwprocess_pb2.ControlImplicitEigenvalueReply(answer = 0)

    def CreateControlImplicitSolution(self,request,context):
        nsolver = request.nsolver
        ilimit = request.ilimit
        maxref = request.maxref
        abstol = request.abstol
        card1 = str(nsolver)+','+str(ilimit)+','+str(maxref)+",0001,0.01,1e10,0.9,"+str(abstol)
        card2 = "2,1,1,3,0,0,0"
        newk = '*CONTROL_IMPLICIT_SOLUTION\n'+card1 +'\n' +card2
        self.kwdproc.newkeyword(newk)
        print('Control Implicit Solution Created...')
        return kwprocess_pb2.ControlImplicitSolutionReply(answer = 0)

    def CreateDBBinary(self,request,context):
        filetype = request.filetype
        dt = request.dt
        card1 = str(dt)+', , , , ,'
        opcode = '*DATABASE_BINARY_'+filetype.upper()
        newk = opcode +'\n' + card1
        self.kwdproc.newkeyword(newk)
        print('DB Binary Created...')
        maxint = request.maxint
        dcomp = request.dcomp
        nintsld = request.nintsld
        ieverp = request.ieverp
        if maxint!=3 or ieverp!=0 or dcomp!=1 or nintsld!=1:
            card1= ",,"+str(maxint)
            card2 = ","+str(ieverp) + ",,"+str(dcomp)
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

    def CreateDBSALE(self,request,context):
        switch = request.switch
        opcode ="*DATABASE_SALE"
        card1 = str(switch)
        newk = opcode +"\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = "*DATABASE_SALE Created..."
        print(msg)
        return kwprocess_pb2.DBSALEReply(answer = 0)

        #INITIAL
    def CreateInitVel(self,request,context):
        nsid = request.nsid
        velocity = request.velocity
        opcode ="*INITIAL_VELOCITY"
        card1 = str(nsid)
        card2 = str(velocity[0])+","+str(velocity[1])+","+str(velocity[2])+","+str(velocity[3])+str(velocity[4])+","+str(velocity[5])
        newk = opcode + "\n" + card1 + "\n" + card2
        self.kwdproc.newkeyword(newk)
        msg = "*INITIAL_VELOCITY Created..."
        print(msg)
        return kwprocess_pb2.InitVelReply(answer = 0)

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

    def CreateInitDetonation(self,request,context):
        pid = request.pid
        coord = request.coord
        lt = request.lt
        opcode ="*INITIAL_DETONATION"
        card1 = str(pid)+","+str(coord[0])+","+str(coord[1])+","+str(coord[2])+","+str(lt)+",,0"
        newk = opcode +"\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = "*INITIAL_DETONATION Created..."
        print(msg)
        return kwprocess_pb2.InitDetonationReply(answer = 0)

    def CreateDefineCurve(self,request,context):
        lcid = request.lcid
        lcid = self.kwdproc.get_data(gdt.KWD_DEFINE_CURVE_LASTID)+1
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
        return kwprocess_pb2.DefineCurveReply(id = lcid)   

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

    def CreateDefineBox(self,request,context):
        boxid = self.kwdproc.get_data(gdt.KWD_DEFINE_BOX_LASTID)+1
        xmin = request.xmin
        xmax = request.xmax
        ymin = request.ymin
        ymax = request.ymax
        zmin = request.zmin
        zmax = request.zmax
        card1 = str(boxid)+","+str(xmin)+","+str(xmax)+","+str(ymin)+","+str(ymax)+","+str(zmin)+","+str(zmax)
        opcode = "*DEFINE_BOX"
        newk = opcode
        newk += "\n" + card1
        self.kwdproc.newkeyword(newk)
        print(f"DefineBox {boxid} Created...")
        return kwprocess_pb2.DefineBoxReply(boxid = boxid) 

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

    def CreateDefineOrientation(self,request,context):
        vid = request.vid
        iop = request.iop
        vector = request.vector
        node1 = request.node1
        node2 = request.node2
        card1 = str(vid)+","+str(iop)+","+str(vector[0])+","+str(vector[1])+","+str(vector[2])+","+str(node1)+","+str(node2)
        opcode = "*DEFINE_SD_ORIENTATION"
        newk = opcode + "\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'Define Orientation '+str(vid)+'Created...'
        print(msg)
        return kwprocess_pb2.DefineOrientationReply(answer = 0)

    def CreatePartSet(self,request,context):
        sid = request.sid
        pids = request.pids
        if sid==0:
            sid = self.kwdproc.get_data(gdt.KWD_PARTSET_LASTID)+1 
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
        return kwprocess_pb2.PartSetReply(id = sid)

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
        if sid==0:
            sid = self.kwdproc.get_data(gdt.KWD_NODESET_LASTID)+1
        opcode = "*SET_NODE"
        if len(option)>0:
            opcode += "_"+option.upper()
        newk = opcode
        card1 = str(sid)
        if option.upper()=="GENERAL":
            card2 = genoption
            for i in range(len(entities)):
                card2 += ","+str(entities[i])
                newk +=  "\n" + card1 + "\n"+card2 
        elif option.upper()=="LIST":
            newk +=  "\n" + card1 + "\n"
            repeatcard = ''
            count = 0
            for nid in entities:
                repeatcard += str(nid)
                count+=1
                if count%8==0 or count >= len(entities):
                    repeatcard += '\n'
                    newk += repeatcard
                    repeatcard=''
                    continue
                repeatcard+=','
        self.kwdproc.newkeyword(newk)
        msg = 'SET_NODE '+str(sid)+' Created...'
        print(msg)
        return kwprocess_pb2.NodeSetReply(id = sid) 

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
            if motion >= 0:
                card4 = str(lcid)+',0,'+str(vx)+','+str(vy)+','+str(vz)
            opcode = '*RIGIDWALL_GEOMETRIC_CYLINDER'
            if motion >= 0:
                opcode+='_MOTION'
            if display != 0:
                opcode += '_DISPLAY'
            newk = opcode+"\n0,0,0\n" + card2 + "\n"+card3
            if motion >=0:
                newk += ('\n'+card4)
            if display !=0:
                newk += "\n ,1e-9,1e-4,0.3"
            self.kwdproc.newkeyword(newk)
        print('Cylinder Rigidwall Geometric Created...')   
        return kwprocess_pb2.RigidWallGeomReply(answer = 0)  

    def CreateRigidWallPlanar(self,request,context):
        nsid = request.nsid
        nsidex = request.nsidex
        boxid = request.boxid
        normal = request.normal
        fric = request.fric
        card1 = str(nsid)+","+str(nsidex)+","+str(boxid)
        card2 = str(normal[0])+","+str(normal[1])+","+str(normal[2])+","+str(normal[3])+","+str(normal[4])+","+str(normal[5])+","+str(fric)
        opcode = "*RIGIDWALL_PLANAR"
        newk = opcode +"\n"+card1+"\n"+card2
        self.kwdproc.newkeyword(newk)
        print('Rigidwall Planar Created...')   
        return kwprocess_pb2.RigidWallPlanarReply(answer = 0) 

    def CreateContact(self,request,context):
        cid = request.cid
        title = request.title
        option1 = request.option1
        option2 = request.option2
        option3 = request.option3
        offset = request.offset
        ssid = request.ssid
        msid = request.msid
        sstyp = request.sstyp
        mstyp = request.mstyp
        sapr = request.sapr
        sbpr = request.sbpr
        #card2
        fs = request.fs
        fd = request.fd
        vdc = request.vdc
        penchk = request.penchk
        birthtime = request.birthtime
        #card3
        sfsa = request.sfsa
        sfsb = request.sfsb
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
        ignore = request.ignore
        opcode = "*CONTACT_"+option1.upper()
        if(len(offset)>0):
            opcode += "_"+offset
        if option3:
            opcode += "_ID"
            card0 = str(cid)+","+title
            opcode += "\n" + card0
        card1 = str(ssid)+","+ str(msid) + "," + str(sstyp) + "," + str(mstyp)+",,,"+str(sapr)+","+str(sbpr);  
        card2 = str(fs)+"," +str(fd)+",,,"+str(vdc)+","+str(penchk)+","+ str(birthtime)
        card3 = str(sfsa)+"," +str(sfsb)+","+str(sst)+"," +str(mst)+",1,1,1,1"
        card4 = str(optionres)+","+str(nfls)+","+str(sfls)+","+str(param)+",,,"+str(ct2cn)
        carda = str(soft)+","+ str(sofscl) + "," + str(lcidab) + "," + str(maxpar)+","+str(sbopt)+","+ str(depth) + "," + str(bsort) + "," + str(frcfrq); 
        cardb = "0,1,2,0,0,0,0.5,0"
        cardc = str(igap)+","+str(ignore)
        if (option1 == "TIED_SHELL_EDGE_TO_SURFACE" or option1 == "NODES_TO_SURFACE"or option1 == "SURFACE_TO_SURFACE"):
            newk = opcode+"\n"+card1+"\n"+card2+"\n"+card3
        if option1 == "AUTOMATIC_SURFACE_TO_SURFACE_TIEBREAK" :
            newk = opcode+"\n"+card1+"\n"+card2+"\n"+card3+"\n"+card4+"\n"+carda
        if option1 == "AUTOMATIC_SINGLE_SURFACE_SMOOTH":
            newk = opcode+"\n"+card1+"\n"+card2+"\n"+card3+"\n"+carda+"\n"+cardb+"\n"+cardc
        if option1 == "AUTOMATIC_SURFACE_TO_SURFACE":
            if option2 == "MORTAR":
                newk = "*CONTACT_"+option1+"_"+option2+"\n" +card1+"\n"+card2+"\n"+card3+"\n"+carda+"\n"+cardb+"\n"+cardc
        if option1 == "AUTOMATIC_SINGLE_SURFACE":
            if option2 == "MORTAR":
                newk = "*CONTACT_"+option1+"_"+option2+"\n" +card1+"\n"+card2+"\n"+card3+"\n"+carda+"\n"+cardb+"\n"+cardc
            else:
                newk = opcode+"\n" + card1+"\n\n\n"+card4
        self.kwdproc.newkeyword(newk)
        print('Contact  Created...')
        return kwprocess_pb2.ContactReply(answer = 0)

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

    def CreateConstrainedNodalRigidBody(self,request,context):
        pid = request.pid
        id = pid + self.kwdproc.get_data(gdt.KWD_PART_LASTID)+1
        nsid = request.nsid
        card1 = str(id) + ",0," + str(nsid) + ",0,0,0,0"
        opcode = "*CONSTRAINED_NODAL_RIGID_BODY"
        newk = opcode+"\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*CNRB Created...'
        print(msg)
        return kwprocess_pb2.ConstrainedNodalRigidBodyReply(pid = id) 

    def CreateConstrainedSpotWeld(self,request,context):
        node1 = request.node1
        node2 = request.node2
        card1 = str(node1) + "," + str(node2) + ",0,0,0,0,0,0"
        opcode = "*CONSTRAINED_SPOTWELD"
        newk = opcode+"\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*CONSTRAINED_SPOTWELD Created...'
        print(msg)
        return kwprocess_pb2.ConstrainedSpotWeldReply(id = 0)

    def CreateConstrainedJoint(self,request,context):
        type = request.type
        nodes = request.nodes
        rps = request.rps
        damp = request.damp
        opcode = "*CONSTRAINED_JOINT_"+type
        card1 = str(nodes[0])
        for i in range(1,len(nodes)):
            card1 += "," + str(nodes[i])
        newk = opcode+"\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*CONSTRAINED_JOINT Created...'
        print(msg)
        return kwprocess_pb2.ConstrainedJointReply(answer = 0) 

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
    def CreateMatEM(self,request,context):
        mid = request.mid
        mtype = request.mtype
        sigma = request.sigma
        card1 = str(mid)+","+str(mtype)+","+str(sigma)
        opcode = "*EM_MAT_001"
        newk = opcode +"\n"+card1
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatEMReply(id = mid)  
        
    def CreateMatRigid(self,request,context):
        mid = self.kwdproc.get_data(gdt.KWD_MAT_LASTID)+1
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
        return kwprocess_pb2.MatRigidReply(mid = mid)  

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
        return kwprocess_pb2.MatElasticReply(mid = mid) 

    def CreateMatSpotweld(self,request,context):
        mid = self.kwdproc.get_data(gdt.KWD_MAT_LASTID)+1
        ro = request.ro
        e = request.e
        pr = request.pr
        sigy = request.sigy
        eh = request.eh
        nrr = request.nrr
        nrs = request.nrs
        nrt = request.nrt
        card1 = str(mid)+","+str(ro)+","+str(e)+","+str(pr)+","+str(sigy)+","+str(eh)
        card2 = "0,"+str(nrr)+","+str(nrs)+","+str(nrt)
        opcode = "*MAT_SPOTWELD"
        newk = opcode +"\n"+card1+"\n"+card2
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatSpotweldReply(mid = mid)  

    def CreateMatPiecewiseLinearPlasticity(self,request,context):
        mid = self.kwdproc.get_data(gdt.KWD_MAT_LASTID)+1
        ro = request.ro
        e = request.e
        pr = request.pr
        sigy = request.sigy
        etan = request.etan
        card1 = str(mid)+","+str(ro)+","+str(e)+","+str(pr)+","+str(sigy)+","+str(etan)
        opcode = "*MAT_PIECEWISE_LINEAR_PLASTICITY"
        newk = opcode +"\n"+card1
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatPiecewiseLinearPlasticityReply(mid = mid)

    def CreateMatModifiedPiecewiseLinearPlasticity(self,request,context):
        mid = self.kwdproc.get_data(gdt.KWD_MAT_LASTID)+1
        ro = request.ro
        e = request.e
        pr = request.pr
        sigy = request.sigy
        etan = request.etan
        fail = request.fail
        numint = request.numint
        card1 = str(mid)+","+str(ro)+","+str(e)+","+str(pr)+","+str(sigy)+","+str(etan)+","+str(fail)
        card2 = "0,0,0,0,0,0,0,"+str(numint)
        opcode = "*MAT_MODIFIED_PIECEWISE_LINEAR_PLASTICITY"
        newk = opcode +"\n"+card1 + "\n"+card2
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatModifiedPiecewiseLinearPlasticityReply(mid = mid)

    def CreateMatFabric(self,request,context):
        mid = request.mid
        ro = request.ro
        ea = request.ea
        eb = request.eb
        prba = request.prba
        prab = request.prab
        gab = request.gab
        card1 = str(mid)+","+str(ro)+","+str(ea)+","+str(eb)+",,"+str(prba)+","+str(prab)
        card2 = str(gab)
        opcode = "*MAT_FABRIC"
        newk = opcode +"\n"+card1+"\n"+card2
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatFabricReply(ret = 0)   

    def CreateMatSpringNonlinearElastic(self,request,context):
        mid = request.mid
        lcid = request.lcid
        card1 = str(mid)+","+str(lcid)
        opcode = "*MAT_SPRING_NONLINEAR_ELASTIC"
        newk = opcode +"\n"+card1
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatSpringNonlinearElasticReply(ret = 0)

    def CreateMatDamperViscous(self,request,context):
        mid = request.mid
        dc = request.dc
        card1 = str(mid)+","+str(dc)
        opcode = "*MAT_DAMPER_VISCOUS"
        newk = opcode +"\n"+card1
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatDamperViscousReply(ret = 0)    

    def CreateMatDamperNonlinearViscous(self,request,context):
        mid = request.mid
        lcdr = request.lcdr
        card1 = str(mid)+","+str(lcdr)
        opcode = "*MAT_DAMPER_NONLINEAR_VISCOUS"
        newk = opcode +"\n"+card1
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatDamperNonlinearViscousReply(ret = 0)

    def CreateMatNull(self,request,context):
        mid = self.kwdproc.get_data(gdt.KWD_MAT_LASTID)+1
        ro = request.ro
        pc = request.pc
        card1 = str(mid)+","+str(ro)+","+str(pc)
        opcode = "*MAT_NULL"
        newk = opcode +"\n"+card1
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatNullReply(mid = mid)

    def CreateMatJohnsonCook(self,request,context):
        mid = self.kwdproc.get_data(gdt.KWD_MAT_LASTID)+1
        ro = request.ro
        g = request.g
        e = request.e
        pr = request.pr
        constants = request.constants
        tm = request.tm
        tr = request.tr
        eps0 = request.eps0
        cp = request.cp
        pc = request.pc
        spall = request.spall
        it = request.it
        failure = request.failure
        card1 = str(mid)+","+str(ro)+","+str(g)+","+str(e)+","+str(pr)
        card2 = str(constants[0])+","+str(constants[1])+","+str(constants[2])+","+str(constants[3])+","+str(constants[4])+","+str(tm)+","+str(tr)+","+str(eps0)
        card3 = str(cp)+","+str(pc)+","+str(spall)+","+str(it)+","+str(failure[0])+","+str(failure[1])+","+str(failure[2])+","+str(failure[3])
        card4 = str(failure[4])
        opcode = "*MAT_JOHNSON_COOK"
        newk = opcode +"\n"+card1+"\n"+card2+"\n"+card3+"\n"+card4
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatJohnsonCookReply(mid = mid)

    def CreateMatHighExplosiveBurn(self,request,context):
        mid = self.kwdproc.get_data(gdt.KWD_MAT_LASTID)+1
        ro = request.ro
        d = request.d
        pcj = request.pcj
        card1 = str(mid)+","+str(ro)+","+str(d)+","+str(pcj)
        opcode = "*MAT_HIGH_EXPLOSIVE_BURN"
        newk = opcode +"\n"+card1
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatHighExplosiveBurnReply(mid = mid)

    def CreateMatVacuum(self,request,context):
        mid = self.kwdproc.get_data(gdt.KWD_MAT_LASTID)+1
        rho = request.rho
        card1 = str(mid)+","+str(rho)
        opcode = "*MAT_VACUUM"
        newk = opcode +"\n"+card1
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatVacuumReply(mid = mid)

    def CreateMatAddErosion(self,request,context):
        mid = request.mid
        mnpres = request.mnpres
        mxeps = request.mxeps
        card1 = str(mid)
        card2 = str(mnpres)+",,,"+str(mxeps)
        opcode = "*MAT_ADD_EROSION"
        newk = opcode +"\n"+card1+"\n"+card2
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.MatAddErosionReply(ret = 0)

    def CreateEOSLinearPolynomial(self,request,context):
        eosid = self.kwdproc.get_data(gdt.KWD_EOS_LASTID)+1
        ci = request.ci
        e0 = request.e0
        v0 = request.v0
        card1 = str(eosid)+","+str(ci[0])+","+str(ci[1])+","+str(ci[2])+","+str(ci[3])+","+str(ci[4])+","+str(ci[5])+","+str(ci[6])
        card2 = str(e0)+","+str(v0)
        opcode = "*EOS_LINEAR_POLYNOMIAL"
        newk = opcode +"\n"+card1+"\n"+card2
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.EOSLinearPolynomialReply(eosid = eosid)

    def CreateEOSJWL(self,request,context):
        eosid = self.kwdproc.get_data(gdt.KWD_EOS_LASTID)+1
        equ = request.jwl_equation
        card1 = str(eosid)+","+str(equ[0])+","+str(equ[1])+","+str(equ[2])+","+str(equ[3])+","+str(equ[4])+","+str(equ[5])+","+str(equ[6])
        opcode = "*EOS_JWL"
        newk = opcode +"\n"+card1
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.EOSJWLReply(eosid = eosid)
  
    def CreateEOSGruneisen(self,request,context):
        eosid = self.kwdproc.get_data(gdt.KWD_EOS_LASTID)+1
        constants = request.constants
        a = request.a
        e0 = request.e0
        card1 = str(eosid)+","+str(constants[0])+","+str(constants[1])+","+str(constants[2])+","+str(constants[3])+","+str(constants[4])+","+str(a)+","+str(e0)
        opcode = "*EOS_GRUNEISEN"
        newk = opcode +"\n"+card1
        self.kwdproc.newkeyword(newk)
        msg = opcode+" Created..."
        print(msg)
        return kwprocess_pb2.EOSGruneisenReply(eosid = eosid)
 
    def CreateSectionIGAShell(self,request,context):
        secid = self.kwdproc.get_data(gdt.KWD_SECTION_LASTID)+1
        elform = request.elform
        shrf = request.shrf
        thickness = request.thickness
        card1 = str(secid) + "," + str(elform) + "," + str(shrf)+", ,"
        card2 = str(thickness)
        newk = "*SECTION_IGA_SHELL\n" + card1+"\n"+card2
        self.kwdproc.newkeyword(newk)
        msg = 'Section IGAShell '+str(secid)+' Created...'
        print(msg)
        return kwprocess_pb2.SectionIGAShellReply(id = secid)

    def CreateSectionBeam(self,request,context):
        secid = self.kwdproc.get_data(gdt.KWD_SECTION_LASTID)+1
        elform = request.elform
        shrf = request.shrf
        cst = request.cst
        ts1 = request.ts1
        ts2 = request.ts2
        card1 = str(secid) + "," + str(elform)+ "," + str(shrf)+ ",2.0," + str(cst)
        card2 = str(ts1)+ "," + str(ts2)
        newk = "*SECTION_BEAM\n" + card1 +"\n"+card2
        self.kwdproc.newkeyword(newk)
        print(f"Section Beam {secid} Created...")
        return kwprocess_pb2.SectionBeamReply(id = secid)

    def CreateSectionShell(self,request,context):
        secid = self.kwdproc.get_data(gdt.KWD_SECTION_LASTID)+1
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
        print(f"Section Shell {secid} Created...")
        return kwprocess_pb2.SectionShellReply(id = secid)
    
    def CreateSectionSolid(self,request,context):
        secid = self.kwdproc.get_data(gdt.KWD_SECTION_LASTID)+1
        elform = request.elform
        card1 = str(secid) + "," + str(elform)
        newk = "*SECTION_SOLID\n" + card1
        self.kwdproc.newkeyword(newk)
        print(f"Section Solid {secid} Created...")
        return kwprocess_pb2.SectionSolidReply(id = secid)

    def CreateSectionDiscrete(self,request,context):
        secid = request.secid
        dro = request.dro
        kd = request.kd
        v0 = request.v0
        cl = request.cl
        fd = request.fd
        cdl = request.cdl
        tdl = request.tdl
        card1 = str(secid) + "," + str(dro)+ "," + str(kd)+ "," + str(v0)+ "," + str(cl)+ "," + str(fd)
        card2 = str(cdl)+"," + str(tdl)
        newk = "*SECTION_DISCRETE\n" + card1 + "\n" + card2
        self.kwdproc.newkeyword(newk)
        msg = 'Section Discrete '+str(secid)+' Created...'
        print(msg)
        return kwprocess_pb2.SectionDiscreteReply(answer = 0)

    def CreateHourglass(self,request,context):
        ghid = self.kwdproc.get_data(gdt.KWD_HOURGLASS_LASTID)+1
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
        return kwprocess_pb2.SectionSolidReply(id = ghid)

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
    
    def SetICFDPartProperty(self,request,context):
        pid = request.pid
        secid = request.secid
        mid = request.mid
        self.kwdproc.set_kwd_data1(gdt.KWD_DETAILDATA_EXTERNALID, "*ICFD_PART", pid,3,secid)
        self.kwdproc.set_kwd_data1(gdt.KWD_DETAILDATA_EXTERNALID, "*ICFD_PART", pid,4,mid)
        msg = 'Set Property for ICFD Part '+str(pid)
        print(msg)
        return kwprocess_pb2.ICFDPartPropertyReply(answer = 0)

    def SetICFDVolumePartProperty(self,request,context):
        pid = request.pid
        secid = request.secid
        mid = request.mid
        self.kwdproc.set_kwd_data(gdt.KWD_DETAILDATA_EXTERNALID, "ICFD_PART_VOL_KIND", pid,3,mid)
        self.kwdproc.set_kwd_data(gdt.KWD_DETAILDATA_EXTERNALID, "ICFD_PART_VOL_KIND", pid,4,secid)
        msg = 'Set Property for ICFD Volume Part '+str(pid)
        print(msg)
        return kwprocess_pb2.ICFDVolumePartPropertyReply(answer = 0)

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
        mid = self.kwdproc.get_data(gdt.KWD_ICFD_MAT_LASTID)+1
        flg = request.flg
        ro = request.ro
        vis = request.vis
        card1 = str(mid) + "," + str(flg)+ "," + str(ro)+ "," + str(vis)
        newk = "*ICFD_MAT\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD material '+str(mid)+' Created...'
        print(msg)
        return kwprocess_pb2.ICFDMatReply(id = mid)    

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
        pid = self.kwdproc.get_data(gdt.KWD_ICFD_PART_VOL_LASTID)+1  
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
        return kwprocess_pb2.ICFDPartVolReply(id = pid) 

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
        volid = self.kwdproc.get_data(gdt.KWD_MESH_VOLUME_LASTID)+1    
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
        return kwprocess_pb2.MeshVolumeReply(id = volid)

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

    def CreateAirbagModel(self,request,context): 
        modeltype = request.modeltype    
        sid = request.sid
        sidtyp = request.sidtyp
        cv = request.cv
        cp = request.cp
        t = request.t
        lcid = request.lcid
        mu = request.mu
        area = request.area
        pe = request.pe
        ro = request.ro
        opcode = "*AIRBAG_"+modeltype
        card1 = str(sid) + "," + str(sidtyp)
        card3 = str(cv) + "," + str(cp)+ ","+str(t) + "," + str(lcid)+ "," + str(mu)+ ","+str(area) + "," + str(pe)+ "," + str(ro)
        newk = opcode+"\n" + card1 + "\n" + card3+"\n0"
        self.kwdproc.newkeyword(newk)
        msg = opcode + ' Created...'
        print(msg)
        return kwprocess_pb2.AirbagModelReply(answer = 0) 

    def CreateEMControl(self,request,context):     
        emsol = request.emsol
        numls = request.numls
        macrodt = request.macrodt
        ncylfem = request.ncylfem
        ncylbem= request.ncylbem
        card1 = str(emsol) + "," + str(numls)+ "," + str(macrodt)+",0,2,,"+str(ncylfem)+ "," + str(ncylbem)
        newk = "*EM_CONTROL\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_CONTROL Created...'
        print(msg)
        return kwprocess_pb2.EMControlReply(answer = 0) 

    def CreateEMTimestep(self,request,context):     
        tstype = request.tstype
        dtconst = request.dtconst
        card1 = str(tstype) + "," + str(dtconst)+ ",,1.0,,,25,0"
        newk = "*EM_CONTROL_TIMESTEP\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_CONTROL_TIMESTEP Created...'
        print(msg)
        return kwprocess_pb2.EMTimestepReply(answer = 0) 

    def CreateEMControlContact(self,request,context):     
        emct = request.emct
        cconly = request.cconly
        ctype = request.ctype
        dtype = request.dtype
        card1 = str(emct) + "," + str(cconly) + "," + str(ctype)+ ","+ str(dtype)+ ",0.3,0.3,0.3,,"
        newk = "*EM_CONTROL_CONTACT\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_CONTROL_CONTACT Created...'
        print(msg)
        return kwprocess_pb2.EMControlContactReply(answer = 0) 

    def CreateEMContact(self,request,context):     
        contid = request.contid
        dtype = request.dtype
        psidm = request.psidm
        psids = request.psids
        eps1 = request.eps1
        eps2 = request.eps2
        eps3 = request.eps3
        d0 = request.d0
        card1 = str(contid) + "," + str(dtype) + "," + str(psidm)+ ","+ str(psids)+","+str(eps1) + "," + str(eps2) + "," + str(eps3)+ ","+ str(d0)
        newk = "*EM_CONTACT\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_CONTACT Created...'
        print(msg)
        return kwprocess_pb2.EMContactReply(answer = 0) 

    def CreateEMCircuitRogo(self,request,context):     
        rogid = request.rogid
        setid = request.setid
        settype = request.settype
        curtyp = request.curtyp
        card1 = str(rogid) + "," + str(setid) + "," + str(settype)+ ","+ str(curtyp)
        newk = "*EM_CIRCUIT_ROGO\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_CIRCUIT_ROGO Created...'
        print(msg)
        return kwprocess_pb2.EMCircuitRogoReply(answer = 0) 

    def CreateEMCircuit(self,request,context):     
        circid = request.circid
        circtyp = request.circtyp
        lcid = request.lcid
        sidcurr = request.sidcurr
        sidvin = request.sidvin
        sidvout = request.sidvout
        card1 = str(circid) + "," + str(circtyp) + "," + str(lcid)+ ",,,,,0.0"
        card2 = str(sidcurr) + "," + str(sidvin) + "," + str(sidvout)
        newk = "*EM_CIRCUIT\n" + card1 + "\n" + card2
        self.kwdproc.newkeyword(newk)
        msg = '*EM_CIRCUIT Created...'
        print(msg)
        return kwprocess_pb2.EMCircuitReply(answer = 0) 

    def CreateEMMat001(self,request,context):     
        mid = request.mid
        mtype = request.mtype
        sigma = request.sigma
        card1 = str(mid) + "," + str(mtype) + "," + str(sigma)+ ",,,,1e28"
        newk = "*EM_MAT_001\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_MAT_001 Created...'
        print(msg)
        return kwprocess_pb2.EMMat001Reply(answer = 0) 

    def CreateEMMat002(self,request,context):     
        mid = request.mid
        mtype = request.mtype
        sigma = request.sigma
        eosid = request.eosid
        murel = request.murel
        card1 = str(mid) + "," + str(mtype) + "," + str(sigma)+","+str(eosid) + "," + str(murel)+ ",0,1e28"
        newk = "*EM_MAT_002\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_MAT_002 Created...'
        print(msg)
        return kwprocess_pb2.EMMat002Reply(answer = 0) 

    def CreateEMSolverBem(self,request,context):     
        reltol = request.reltol
        maxite = request.maxite
        stype = request.stype
        precon = request.precon
        uselast = request.uselast
        ncylbem = request.ncylbem
        card1 = str(reltol) +","+str(maxite)+","+str(stype)+","+str(precon)+","+str(uselast)+","+str(ncylbem)
        newk = "*EM_SOLVER_BEM\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_SOLVER_BEM Created...'
        print(msg)
        return kwprocess_pb2.EMSolverBemReply(answer = 0) 

    def CreateEMSolverFem(self,request,context):     
        reltol = request.reltol
        maxite = request.maxite
        stype = request.stype
        precon = request.precon
        uselast = request.uselast
        ncylbem = request.ncylbem
        card1 = str(reltol) +","+str(maxite)+","+str(stype)+","+str(precon)+","+str(uselast)+","+str(ncylbem)
        newk = "*EM_SOLVER_FEM\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_SOLVER_FEM Created...'
        print(msg)
        return kwprocess_pb2.EMSolverFemReply(answer = 0) 

    def CreateEMSolverBemMat(self,request,context):     
        matid = request.matid
        card1 = str(matid) + ",,,,,,,1e-6"
        newk = "*EM_SOLVER_BEMMAT\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_SOLVER_BEMMAT Created...'
        print(msg)
        return kwprocess_pb2.EMSolverBemMatReply(answer = 0) 

    def CreateEMSolverFemBemMonolithic(self,request,context):     
        mtype = request.mtype
        stype = request.stype
        abstol = request.abstol
        reltol = request.reltol
        maxit = request.maxit
        card1 = str(mtype) + ","+str(stype)+ ","+str(abstol)+ ","+str(reltol)+ ","+str(maxit)
        newk = "*EM_SOLVER_FEMBEM_MONOLITHIC\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_SOLVER_FEMBEM_MONOLITHIC Created...'
        print(msg)
        return kwprocess_pb2.EMSolverFemBemMonolithicReply(answer = 0)

    def CreateEMOutput(self,request,context):     
        mats = request.mats
        matf = request.matf
        sols = request.sols
        solf = request.solf
        card1 = str(mats)+ "," + str(matf)+ "," + str(sols)+ ","+ str(solf)+",0,0,0"
        newk = "*EM_OUTPUT\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_OUTPUT Created...'
        print(msg)
        return kwprocess_pb2.EMOutputReply(answer = 0) 

    def CreateEMDatabaseGlobalEnergy(self,request,context):     
        outlv = request.outlv
        card1 = str(outlv)+",0.0"
        newk = "*EM_DATABASE_GLOBALENERGY\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_DATABASE_GLOBALENERGY Created...'
        print(msg)
        return kwprocess_pb2.EMDatabaseGlobalEnergyReply(answer = 0)

    def CreateEMPermanentMagnet(self,request,context):     
        id = request.id
        partid = request.partid
        mtype = request.mtype
        north = request.north
        sourth = request.sourth
        hc = request.hc
        card1 = str(id)+","+str(partid)+","+str(mtype)+","+str(north)+","+str(sourth)+","+str(hc)
        newk = "*EM_PERMANENT_MAGNET\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_PERMANENT_MAGNET Created...'
        print(msg)
        return kwprocess_pb2.EMPermanentMagnetReply(answer = 0)

    def CreateEMEOSPermeability(self,request,context):     
        eosid = request.eosid
        eostype = request.eostype
        lcid = request.lcid
        card1 = str(eosid)+","+str(eostype)+","+str(lcid)
        newk = "*EM_EOS_PERMEABILITY\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*EM_EOS_PERMEABILITY Created...'
        print(msg)
        return kwprocess_pb2.EMEOSPermeabilityReply(answer = 0)

    def ALECreateStructuredMesh(self,request,context):     
        nbid = request.nbid
        ebid = request.ebid
        cpidx = request.cpidx
        cpidy = request.cpidy
        cpidz = request.cpidz
        mshid = self.kwdproc.get_data(gdt.KWD_ALE_STRUCTURED_MESH_LASTID)+1
        lastpid = self.kwdproc.get_data(gdt.KWD_PART_LASTID)+1
        card1 = str(mshid)+","+str(lastpid)+","+str(nbid)+","+str(ebid)
        card2 = str(cpidx)+","+str(cpidy)+","+str(cpidz)
        newk = "*ALE_STRUCTURED_MESH\n" + card1 + "\n" + card2
        self.kwdproc.newkeyword(newk)
        msg = '*ALE_STRUCTURED_MESH Created...'
        print(msg)
        return kwprocess_pb2.ALECreateStructuredMeshReply(meshid = mshid,partid = lastpid)

    def ALECreateStructuredMeshCtrlPoints(self,request,context):     
        icase = request.icase
        sfo = request.sfo
        n = request.n
        x = request.x
        ratio = request.ratio
        cpid = self.kwdproc.get_data(gdt.KWD_ALE_STRUCTURED_MESH_CONTROL_POINTS_LASTID)+1
        card1 = str(cpid)+",,"+str(icase)+","+str(sfo)
        size=20
        newk = "*ALE_STRUCTURED_MESH_CONTROL_POINTS\n" + card1
        for i in range(len(n)):
            if x[i]==0:
                xstr=" "
            else:
                xstr=str(x[i])
            newk += "\n"+str(n[i]).rjust(size,' ')+xstr.rjust(size,' ')+str(ratio[i]).rjust(size,' ')
        self.kwdproc.newkeyword(newk)
        msg = '*ALE_STRUCTURED_MESH_CONTROL_POINTS Created...'
        print(msg)
        return kwprocess_pb2.ALECreateStructuredMeshControlPointsReply(cpid = cpid)    
    
    def ALECreateStructuredMeshRefine(self,request,context):     
        mshid = request.mshid
        ifx = request.ifx
        ify = request.ify
        ifz = request.ifz
        card1 = str(mshid)+","+str(ifx)+","+str(ify)+","+str(ifz)
        newk = "*ALE_STRUCTURED_MESH_REFINE\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*ALE_STRUCTURED_MESH_REFINE Created...'
        print(msg)
        return kwprocess_pb2.ALECreateStructuredMeshRefineReply(answer = 0)

    def ALECreateControl(self,request,context):     
        dct = request.dct
        nadv = request.nadv
        meth = request.meth
        afac = request.afac
        end = request.end
        aafac = request.aafac
        vfact = request.vfact
        pref = request.pref
        card1 = str(dct)+","+str(nadv)+","+str(meth)+","+str(afac)+",0,0,0,0"
        card2 = "0,"+str(end)+","+str(aafac)+","+str(vfact)+",0,0,"+str(pref)
        newk = "*CONTROL_ALE\n" + card1 +"\n"+card2
        self.kwdproc.newkeyword(newk)
        msg = '*CONTROL_ALE Created...'
        print(msg)
        return kwprocess_pb2.ControlALEReply(answer = 0)

    def ALECreateStructuredMultiMaterialGroup(self,request,context):     
        nmmgnm = request.nmmgnm
        mid = request.mid
        eosid = request.eosid
        pref = request.pref
        card1 = str(nmmgnm)+","+str(mid)+","+str(eosid)+",,,,,"+str(pref)+",0,0,0,0"
        newk = "*ALE_STRUCTURED_MULTI-MATERIAL_GROUP\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = '*ALE_STRUCTURED_MULTI-MATERIAL_GROUP Created...'
        print(msg)
        return kwprocess_pb2.ALECreateStructuredMultiMatGroupReply(answer = 0)

    def ALECreateStructuredMeshVolumeFilling(self,request,context):     
        mshid = request.mshid
        ammgto = request.ammgto
        nsample = request.nsample
        vid = request.vid
        geom = request.geom
        inout = request.inout
        e = request.e
        card1 = str(mshid)+",,"+str(ammgto)+",,"+str(nsample)+",,,"+str(vid)
        card2 = str(geom)+","+str(inout)
        for i in range(min(len(e),5)):
            para=int(e[i])
            card2 += (","+str(para))
        newk = "*ALE_STRUCTURED_MESH_VOLUME_FILLING\n" + card1 +"\n"+card2
        self.kwdproc.newkeyword(newk)
        msg = '*ALE_STRUCTURED_MESH_VOLUME_FILLING Created...'
        print(msg)
        return kwprocess_pb2.ALECreateStructuredMeshVolumeFillingReply(answer = 0)

    def CreateGeneralKWD(self,request,context): 
        opcode = request.opcode    
        keyworddata = request.keyworddata
        if opcode[0]!='*':
            opcode = "*"+opcode
        newk = opcode +"\n"+keyworddata
        self.kwdproc.newkeyword(newk)
        msg = opcode + ' Created...'
        print(msg)
        return kwprocess_pb2.GeneralKWDReply(answer = 0) 

if __name__ == '__main__':
    server = IGAServer()
    server.run()