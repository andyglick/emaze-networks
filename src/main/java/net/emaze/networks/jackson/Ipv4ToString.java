package net.emaze.networks.jackson;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import java.io.IOException;
import net.emaze.networks.ipv4.Ipv4;

public class Ipv4ToString extends JsonSerializer<Ipv4> {

    @Override
    public void serialize(Ipv4 ip, JsonGenerator jg, SerializerProvider sp) throws IOException, JsonProcessingException {
        jg.writeString(ip.toString());
    }
}
