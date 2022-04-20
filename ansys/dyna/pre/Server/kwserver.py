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
        firstcard = str(endtim)
        newk = '*CONTROL_TERMINATION\n'+ firstcard
        self.kwdproc.newkeyword(newk)
        print('Termination Created...')
        return kwprocess_pb2.TerminationReply(answer = 0)

    def CreateContact(self,request,context):
        rwpnal = request.rwpnal
        ignore = request.ignore
        igactc = request.igactc
        card1 = '0,'+str(rwpnal)
        card4 = str(ignore)
        card6 = '0, , , , , , ,'+str(igactc)
        newk = '*CONTROL_CONTACT\n'+card1+'\n\n\n'+card4+'\n\n'+card6
        self.kwdproc.newkeyword(newk)
        print('Contact Created...')
        return kwprocess_pb2.ContactReply(answer = 0)

    def CreateDBBinary(self,request,context):
        dt = request.dt
        card1 = str(dt)+', , , , ,'
        newk = '*DATABASE_BINARY_D3PLOT\n' + card1
        self.kwdproc.newkeyword(newk)
        print('DB Binary Created...')
        return kwprocess_pb2.DBBinaryReply(answer = 0)

    def CreateDefineCurve(self,request,context):
        lcid = request.lcid
        sfo = request.sfo
        abscissa = request.abscissa
        ordinate = request.ordinate
        card1 = str(lcid)+', , ,'+str(sfo)
        newk = '*DEFINE_CURVE\n'+card1+"\n"
        for index in range(len(abscissa)):
            repeatcard = str(abscissa[index]) + "," + str(ordinate[index])+"\n"
            newk += repeatcard
        self.kwdproc.newkeyword(newk)
        msg = 'DefineCurve '+str(lcid)+'Created...'
        print(msg)
        return kwprocess_pb2.DefineCurveReply(answer = 0)   

    def CreatePartSet(self,request,context):
        sid = request.sid
        pids = request.pids
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
        return kwprocess_pb2.PartSetReply(answer = 0)
    
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

    def CreateSectionIGAShell(self,request,context):
        secid = request.secid
        elform = request.elform
        shrf = request.shrf
        thickness = request.thickness
        card1 = str(secid) + "," + str(elform) + "," + str(shrf)
        card2 = str(thickness)
        newk = "*SECTION_IGA_SHELL\n" + card1+"\n"+card2
        self.kwdproc.newkeyword(newk)
        msg = 'Section IGAShell '+str(secid)+'Created...'
        print(msg)
        return kwprocess_pb2.SectionIGAShellReply(answer = 0)
    
    def CreateSectionSolid(self,request,context):
        secid = request.secid
        elform = request.elform
        card1 = str(secid) + "," + str(elform)
        newk = "*SECTION_SOLID\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'Section Solid '+str(secid)+'Created...'
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
        msg = 'Hourglass '+str(ghid)+'Created...'
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
        card1 = str(tim) + "," + str(dt)
        newk = "*ICFD_CONTROL_TIME\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'ICFD Control time Created...'
        print(msg)
        return kwprocess_pb2.ICFDControlTimeReply(answer = 0) 

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
        card1 = str(pid) + "," + str(dof)+ "," + str(vad)+ "," + str(lcid)
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

    def MESHCreateBl(self,request,context):     
        pid = request.pid
        nelth = request.nelth
        card1 = str(pid) + "," + str(nelth)
        newk = "*MESH_BL\n" + card1
        self.kwdproc.newkeyword(newk)
        msg = 'MESH bl '+str(pid)+' Created...'
        print(msg)
        return kwprocess_pb2.MeshBlReply(answer = 0)   

if __name__ == '__main__':
    server = IGAServer()
    server.run()